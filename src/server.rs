use std::collections::HashMap;
use std::error::Error;

use lsp_server as lsp;
use lsp_types::notification::{Notification, PublishDiagnostics};
use lsp_types::request::GotoDefinition;
use lsp_types::request::Request;
use lsp_types::*;
use ropey::Rope;

use crate::driver::Driver;
use crate::term::*;

pub struct Server {
    io_threads: lsp::IoThreads,
    connection: lsp::Connection,
    initialization_params: InitializeParams,
    file_ids: HashMap<Url, FileId>,
    source: HashMap<Url, Rope>,
    bindings: Bindings,
    driver: Driver,
    // db: DatabaseImpl,
}
impl Server {
    pub fn start() -> Self {
        // Note that  we must have our logging only write out to stderr.
        eprintln!("starting generic LSP server");

        // Create the transport. Includes the stdio (stdin and stdout) versions but this could
        // also be implemented to use sockets or HTTP.
        let (connection, io_threads) = lsp::Connection::stdio();

        // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
        let server_capabilities = serde_json::to_value(&ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::INCREMENTAL,
            )),
            definition_provider: Some(OneOf::Left(true)),
            completion_provider: Some(CompletionOptions {
                trigger_characters: Some(vec!['.'.into(), ':'.into()]),
                completion_item: Some(CompletionOptionsCompletionItem {
                    label_details_support: Some(true),
                }),
                ..CompletionOptions::default()
            }),
            ..Default::default()
        })
        .unwrap();
        let initialization_params = connection.initialize(server_capabilities).unwrap();
        let initialization_params: InitializeParams =
            serde_json::from_value(initialization_params).unwrap();
        eprintln!("{:?}", initialization_params.capabilities);

        Server {
            source: HashMap::new(),
            file_ids: HashMap::new(),
            io_threads,
            connection,
            initialization_params,
            bindings: Default::default(),
            driver: Driver::new(Vec::new(), &mut Bindings::default()),
            // db: DatabaseImpl::default(),
        }
    }

    pub fn shutdown(self) {
        self.io_threads.join().unwrap();
    }

    pub fn main_loop(&mut self) {
        while let Ok(msg) = self.connection.receiver.recv() {
            eprintln!("Got message!");
            match msg {
                lsp::Message::Request(msg) => self.handle_request(msg),
                lsp::Message::Response(msg) => self.handle_response(msg),
                lsp::Message::Notification(msg) => self.handle_notification(msg),
            }
            .unwrap_or_else(|e| {
                eprintln!("WARNING: error in handling message: {}", e);
            })
        }
    }

    fn handle_response(&mut self, msg: lsp::Response) -> Result<(), Box<dyn Error>> {
        eprintln!("Ignoring response");
        Ok(())
    }

    fn handle_request(&mut self, msg: lsp::Request) -> Result<(), Box<dyn Error>> {
        if self.connection.handle_shutdown(&msg).unwrap() {
            eprintln!("Shutdown request received");
            return Ok(());
        }

        match &*msg.method {
            request::Completion::METHOD => {
                let (id, params): (_, lsp_types::CompletionParams) =
                    msg.extract(request::Completion::METHOD)?;
                let url = &params.text_document_position.text_document.uri;
                let file = self.file_ids[&params.text_document_position.text_document.uri];
                let pos = params.text_document_position.position;
                if let Some(completions) = self.find_completions(url, pos, file) {
                    let result = CompletionResponse::Array(completions);
                    let result = serde_json::to_value(&result)?;
                    let resp = lsp::Response {
                        id,
                        result: Some(result),
                        error: None,
                    };
                    self.connection.sender.send(lsp::Message::Response(resp))?;
                    return Ok(());
                }
                let resp = lsp::Response {
                    id,
                    result: Some(serde_json::Value::Null),
                    error: None,
                };
                self.connection.sender.send(lsp::Message::Response(resp))?;
            }
            GotoDefinition::METHOD => {
                eprintln!("Handling GoToDefinition");
                let (id, params): (_, GotoDefinitionParams) =
                    msg.extract(GotoDefinition::METHOD)?;
                // example go to definition handler
                let result = GotoDefinitionResponse::Array(Vec::new());
                let result = serde_json::to_value(&result)?;
                let resp = lsp::Response {
                    id,
                    result: Some(result),
                    error: None,
                };
                self.connection.sender.send(lsp::Message::Response(resp))?;
            }
            _ => eprintln!("Ignored unknown request {}", msg.method),
        }

        Ok(())
    }

    fn find_completions(
        &mut self,
        url: &Url,
        pos: Position,
        file: FileId,
    ) -> Option<Vec<CompletionItem>> {
        let source = self.source.get(url).unwrap();
        let pos =
            source.char_to_byte(source.line_to_char(pos.line as usize) + pos.character as usize);
        if let Some(module) = self.driver.mods.iter().find(|m| m.file == file) {
            // Identify the last item that starts before the cursor, and assume we're in that one
            let mut item = None;
            for m in module.items.iter() {
                if m.span().0 <= pos {
                    match item {
                        None => item = Some(m),
                        Some(i) if i.span().0 < m.span().0 => item = Some(m),
                        Some(_) => (),
                    }
                }
            }
            if let Some(item) = item {
                let mut found_member = None;
                let mut is_call = false;
                let mut found_array = None;
                item.visit(&mut |x| match x {
                    Term::Member(_, t, m) if m.span.0 <= pos + 1 && m.span.1 + 1 >= pos => {
                        found_member = Some(*t)
                    }
                    Term::Call(_, Err(m), _) if m.span.0 <= pos + 1 && m.span.1 + 1 >= pos => {
                        found_member = Some(**m);
                        is_call = true
                    }
                    Term::ArrayMethod(_, ArrayMethod::Unknown(span, b))
                        if span.0 <= pos + 1 && span.1 + 1 >= pos =>
                    {
                        found_array = Some(*b)
                    }
                    _ => (),
                });
                if let Some(found) = found_member {
                    let cxt = crate::elaborate::Cxt::from_type(
                        module.ty.clone(),
                        Vec::new(),
                        &mut self.bindings,
                        file,
                        module.mod_path.clone(),
                        &mut self.driver,
                    );

                    let info = cxt.class_info(found).clone();
                    let members = info.members.iter().map(|(s, _, t)| CompletionItem {
                        label: self.bindings.resolve_raw(*s).into(),
                        kind: Some(CompletionItemKind::PROPERTY),
                        detail: Some(t.pretty(&self.bindings).raw_string()),
                        label_details: Some(CompletionItemLabelDetails {
                            detail: None,
                            description: Some(t.pretty(&self.bindings).raw_string()),
                        }),
                        ..Default::default()
                    });
                    let methods = info
                        .methods
                        .iter()
                        .map(|(s, _, t)| Self::make_fn_comp_item(&self.bindings, s, t, !is_call));
                    return Some(members.chain(methods).collect());
                } else if let Some(sarr) = found_array {
                    return Some(
                        if sarr {
                            [("len()", "fn(): i32")].iter()
                        } else {
                            [
                                ("push(…)", "fn(T)"),
                                ("pop()", "fn(): T"),
                                ("len()", "fn(): i32"),
                                ("clear()", "fn()"),
                            ]
                            .iter()
                        }
                        .map(|(s, t)| CompletionItem {
                            label: s.to_string(),
                            kind: Some(CompletionItemKind::METHOD),
                            detail: Some(t.to_string()),
                            label_details: Some(CompletionItemLabelDetails {
                                detail: None,
                                description: Some(t.to_string()),
                            }),
                            ..Default::default()
                        })
                        .collect(),
                    );
                } else {
                    // Try to complete paths
                    // let rope = self.source.get(&Url::from_file_path(&module.input_path).unwrap()).unwrap();
                    let pos = source.byte_to_char(pos);
                    let mut ppos = pos - 1;
                    while source.char(ppos).is_alphanumeric() || source.char(ppos) == ':' {
                        ppos -= 1;
                    }
                    let path = source.slice(ppos + 1..pos).to_string();
                    eprintln!("Completion backup: {}", path);
                    let mut path: Vec<_> = path
                        .split("::")
                        .map(|x| Spanned::hack(self.bindings.raw(x.trim())))
                        .collect();
                    if let Some(_stem) = path.pop() {
                        // TODO better path system
                        if !path.is_empty() {
                            let mut cxt = crate::elaborate::Cxt::from_type(
                                module.ty.clone(),
                                Vec::new(),
                                &mut self.bindings,
                                file,
                                module.mod_path.clone(),
                                &mut self.driver,
                            );

                            let n = path.pop().unwrap();
                            let path = RawPath(path, n);
                            if let Some(m) = cxt.module(&path).cloned() {
                                // Try to suggest paths from a module
                                let classes = m.classes.iter().map(|(s, (_, _))| CompletionItem {
                                    label: cxt.bindings.resolve_raw(*s.last()).into(),
                                    kind: Some(CompletionItemKind::CLASS),
                                    detail: Some("class".into()),
                                    label_details: Some(CompletionItemLabelDetails {
                                        detail: None,
                                        description: Some("class".into()),
                                    }),
                                    ..Default::default()
                                });
                                let vars = m.vars.iter().map(|(s, _, (t, _))| CompletionItem {
                                    label: cxt.bindings.resolve_raw(*s).into(),
                                    kind: Some(CompletionItemKind::VARIABLE),
                                    detail: Some(t.pretty(&cxt.bindings).raw_string()),
                                    label_details: Some(CompletionItemLabelDetails {
                                        detail: None,
                                        description: Some(t.pretty(&cxt.bindings).raw_string()),
                                    }),
                                    ..Default::default()
                                });
                                let fns = m.fns.iter().map(|(s, _, t)| {
                                    Self::make_fn_comp_item(
                                        &cxt.bindings,
                                        s,
                                        t,
                                        source.char(pos) != '(',
                                    )
                                });
                                return Some(fns.chain(classes).chain(vars).collect());
                            } else {
                                // Try to suggest variants of an enum
                                if let Some(class) = cxt.class(&path) {
                                    if let Some(variants) = cxt.class_info(class).variants.clone() {
                                        let variants = variants.into_iter().map(|(s, p)| {
                                            if p.is_empty() {
                                                CompletionItem {
                                                    label: self.bindings.resolve_raw(s).into(),
                                                    kind: Some(CompletionItemKind::VARIABLE),
                                                    detail: Some(
                                                        self.bindings
                                                            .resolve_raw(*path.last())
                                                            .into(),
                                                    ),
                                                    label_details: Some(
                                                        CompletionItemLabelDetails {
                                                            detail: None,
                                                            description: Some(
                                                                self.bindings
                                                                    .resolve_raw(*path.last())
                                                                    .into(),
                                                            ),
                                                        },
                                                    ),
                                                    ..Default::default()
                                                }
                                            } else {
                                                let detail = Some(
                                                    Doc::start("(")
                                                        .chain(Doc::intersperse(
                                                            p.into_iter()
                                                                .map(|t| t.pretty(&self.bindings)),
                                                            Doc::start(", "),
                                                        ))
                                                        .add("): ")
                                                        .add(
                                                            self.bindings.resolve_raw(*path.last()),
                                                        )
                                                        .raw_string(),
                                                );
                                                CompletionItem {
                                                    label: format!(
                                                        "{}(…)",
                                                        self.bindings.resolve_raw(s)
                                                    ),
                                                    kind: Some(CompletionItemKind::FUNCTION),
                                                    detail: detail.clone(),
                                                    label_details: Some(
                                                        CompletionItemLabelDetails {
                                                            detail: None,
                                                            description: detail,
                                                        },
                                                    ),
                                                    insert_text: Some(format!(
                                                        "{}($0)",
                                                        self.bindings.resolve_raw(s)
                                                    )),
                                                    insert_text_format: Some(
                                                        InsertTextFormat::SNIPPET,
                                                    ),
                                                    ..Default::default()
                                                }
                                            }
                                        });
                                        return Some(variants.collect());
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        None
    }

    fn make_fn_comp_item(
        bindings: &Bindings,
        s: &RawSym,
        t: &FnType,
        gen_args: bool,
    ) -> CompletionItem {
        if t.0.is_empty() {
            CompletionItem {
                label: format!("{}()", bindings.resolve_raw(*s)),
                kind: Some(CompletionItemKind::METHOD),
                detail: Some(t.pretty(bindings).raw_string()),
                insert_text: (!gen_args).then(|| bindings.resolve_raw(*s).into()),
                documentation: None,
                label_details: Some(CompletionItemLabelDetails {
                    // This field shows up left-aligned after the label, Rust uses it for e.g. `(as Clone)`
                    detail: None,
                    description: Some(t.pretty(&bindings).raw_string()),
                }),
                preselect: Some(true),
                ..Default::default()
            }
        } else {
            CompletionItem {
                label: format!("{}(…)", bindings.resolve_raw(*s)),
                kind: Some(CompletionItemKind::METHOD),
                detail: Some(t.pretty(&bindings).raw_string()),
                insert_text: Some(
                    Doc::start(bindings.resolve_raw(*s))
                        .chain(if gen_args {
                            Doc::start('(')
                                .chain(Doc::intersperse(
                                    t.0.iter().enumerate().map(|(i, (n, _))| {
                                        Doc::start("${")
                                            .add(i + 1)
                                            .add(":")
                                            .add(bindings.resolve_raw(*n))
                                            .add("}")
                                    }),
                                    Doc::start(", "),
                                ))
                                .add(')')
                        } else {
                            Doc::none()
                        })
                        .raw_string(),
                ),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                documentation: None,
                label_details: Some(CompletionItemLabelDetails {
                    // This field shows up left-aligned after the label, Rust uses it for e.g. `(as Clone)`
                    detail: None,
                    description: Some(t.pretty(&bindings).raw_string()),
                }),
                preselect: Some(true),
                ..Default::default()
            }
        }
    }

    fn handle_notification(&mut self, msg: lsp::Notification) -> Result<(), Box<dyn Error>> {
        use lsp_types::notification::*;
        match &*msg.method {
            DidOpenTextDocument::METHOD => {
                eprintln!("Received DidOpenTextDocument");
                let params: DidOpenTextDocumentParams = msg.extract(DidOpenTextDocument::METHOD)?;
                let file = params.text_document.uri;
                // let file = self.db.file_id(FileLoc::Url(file));
                let source = params.text_document.text.into();
                self.source.insert(file.clone(), source);
                self.signal_change(file);
            }
            DidChangeTextDocument::METHOD => {
                eprintln!("Received DidChangeTextDocument");
                let params: DidChangeTextDocumentParams =
                    msg.extract(DidChangeTextDocument::METHOD)?;
                let file = params.text_document.uri;
                // let file = self.db.file_id(FileLoc::Url(file));
                let source = self.source.get_mut(&file).unwrap();
                for change in params.content_changes {
                    if let Some(range) = change.range {
                        let start = source.line_to_char(range.start.line as usize)
                            + range.start.character as usize;
                        let end = source.line_to_char(range.end.line as usize)
                            + range.end.character as usize;
                        source.remove(start..end);
                        source.insert(start, &change.text);
                    } else {
                        // If range is None then it's the whole document
                        *source = Rope::from(change.text);
                    }
                }
                self.signal_change(file);
            }
            _ => eprintln!("Ignored unknown notification {}", msg.method),
        }

        Ok(())
    }

    fn signal_change(&mut self, file_changed: Url) {
        // Publish diagnostics (example)
        eprintln!("Starting diagnostic publishing...");

        if !self.file_ids.contains_key(&file_changed) {
            let path = file_changed.to_file_path().unwrap();
            if let Some(m) = self.driver.mods_base.iter().find(|m| m.input_path == path) {
                self.file_ids.insert(file_changed.clone(), m.file);
            } else {
                let mod_name = self.bindings.raw(
                    file_changed
                        .path_segments()
                        .unwrap()
                        .next_back()
                        .unwrap()
                        .split('.')
                        .next()
                        .unwrap(),
                );
                eprintln!("module {}", self.bindings.resolve_raw(mod_name));
                self.file_ids
                    .insert(file_changed.clone(), FileId(self.driver.nfiles));
                self.driver.nfiles += 1;
            }
            INPUT_PATH.write().unwrap().insert(
                self.file_ids[&file_changed],
                file_changed.to_file_path().unwrap(),
            );
        }

        INPUT_SOURCE.write().unwrap().insert(
            self.file_ids[&file_changed],
            self.source[&file_changed].clone(),
        );

        eprintln!(
            "Updated: {} (file {:?})",
            file_changed, self.file_ids[&file_changed]
        );
        self.driver.update_mod_src(
            self.file_ids[&file_changed],
            &mut self.bindings,
            &self.source[&file_changed],
        );

        self.publish_diagnostics();
    }

    fn publish_diagnostics(&mut self) {
        let mut diagnostics: HashMap<FileId, Vec<Diagnostic>> =
            self.file_ids.values().map(|&k| (k, Vec::new())).collect();
        self.driver.run_all(&mut self.bindings);
        self.driver.stage = 0;
        for (error, file) in self.driver.errors.split_off(0) {
            if let Some((url, _)) = self.file_ids.iter().find(|(_, v)| **v == file) {
                diagnostics.entry(file).or_default().push(self.lsp_error(
                    error,
                    Severity::Error,
                    url,
                ));
            }
        }
        // TODO only send diagnostics if they changed
        for (file, diagnostics) in diagnostics {
            let message = lsp::Notification {
                method: PublishDiagnostics::METHOD.to_string(),
                params: serde_json::to_value(&PublishDiagnosticsParams {
                    uri: self
                        .file_ids
                        .iter()
                        .find(|(_, v)| **v == file)
                        .unwrap()
                        .0
                        .clone(),
                    version: None,
                    diagnostics,
                })
                .unwrap(),
            };
            self.connection
                .sender
                .send(lsp::Message::Notification(message))
                .unwrap();
        }

        eprintln!("Published diagnostics!");
    }

    fn lsp_error(
        &self,
        error: Spanned<Doc>,
        severity: Severity,
        file: &Url,
    ) -> lsp_types::Diagnostic {
        lsp_types::Diagnostic {
            range: error.span.lsp_range(&self.source[file]),
            severity: Some(severity.lsp()),
            code: None,
            code_description: None,
            source: None,
            message: error.inner.raw_string(),
            // TODO: in some cases we may also want separate NOTE-type diagnostics for secondary labels?
            related_information: None,
            // Some(
            //     self.secondary
            //         .into_iter()
            //         .map(|x| lsp_types::DiagnosticRelatedInformation {
            //             location: lsp_types::Location {
            //                 uri: db.lookup_file_id(split_span.0).to_url().unwrap(),
            //                 range: split_span.add(x.span).lsp_range(files),
            //             },
            //             message: x.message.to_string(false),
            //         })
            //         .collect(),
            // ),
            // TODO: this is useful for deprecated or unnecessary code
            tags: None,
            data: None,
        }
    }
}
