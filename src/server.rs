use std::collections::HashMap;
use std::error::Error;

use lsp_server as lsp;
use lsp_types::notification::{Notification, PublishDiagnostics};
use lsp_types::request::GotoDefinition;
use lsp_types::request::Request;
use lsp_types::*;
use ropey::Rope;

use crate::driver::Driver;
use crate::parser::Parser;
use crate::term::*;

pub struct Server {
    io_threads: lsp::IoThreads,
    connection: lsp::Connection,
    initialization_params: InitializeParams,
    file_ids: HashMap<Url, FileId>,
    source: HashMap<Url, Rope>,
    parsed: HashMap<Url, Vec<PreItem>>,
    parse_errors: HashMap<FileId, Vec<lsp_types::Diagnostic>>,
    bindings: Bindings,
    last_mods: Vec<crate::driver::ModStatus>,
    last_mod_tys: HashMap<RawSym, crate::term::ModType>,
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
                trigger_characters: Some(vec!['.'.into()]),
                ..CompletionOptions::default()
            }),
            ..Default::default()
        })
        .unwrap();
        let initialization_params = connection.initialize(server_capabilities).unwrap();
        let initialization_params: InitializeParams =
            serde_json::from_value(initialization_params).unwrap();

        Server {
            source: HashMap::new(),
            file_ids: HashMap::new(),
            parsed: HashMap::new(),
            parse_errors: HashMap::new(),
            io_threads,
            connection,
            initialization_params,
            bindings: Default::default(),
            last_mods: Vec::new(),
            last_mod_tys: Default::default(),
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
                let source = self.source.get(url).unwrap();
                let pos = source
                    .char_to_byte(source.line_to_char(pos.line as usize) + pos.character as usize);
                if let Some(module) = self.last_mods.iter().find(|m| m.file == file) {
                    if let Some(item) = module
                        .items
                        .iter()
                        .enumerate()
                        .find(|(_, m)| m.span().0 > pos)
                        .and_then(|(i, m)| {
                            if i == 0 {
                                Some(m)
                            } else {
                                module.items.get(i - 1)
                            }
                        })
                        .or_else(|| module.items.last())
                    {
                        let mut found = None;
                        item.visit(&mut |x| match x {
                            Term::Member(_, t, m) if m.span.0 <= pos + 1 && m.span.1 + 1 >= pos => {
                                found = Some(*t)
                            }
                            _ => (),
                        });
                        if let Some(found) = found {
                            let cxt = crate::elaborate::Cxt::from_type(
                                module.ty.clone(),
                                self.last_mod_tys.clone(),
                                Vec::new(),
                                &mut self.bindings,
                                file,
                            );
                            let info = cxt.class_info(found).clone();
                            let members = info.members.iter().map(|(s, _, t)| CompletionItem {
                                label: self.bindings.resolve_raw(*s).into(),
                                kind: Some(CompletionItemKind::PROPERTY),
                                detail: Some(t.pretty(&self.bindings).raw_string()),
                                ..Default::default()
                            });
                            let methods = info.methods.iter().map(|(s, _, t)| CompletionItem {
                                label: format!("{}(...)", self.bindings.resolve_raw(*s)),
                                kind: Some(CompletionItemKind::METHOD),
                                detail: Some(t.pretty(&self.bindings).raw_string()),
                                insert_text: Some(format!("{}($0)", self.bindings.resolve_raw(*s))),
                                insert_text_format: Some(InsertTextFormat::SNIPPET),
                                ..Default::default()
                            });
                            let result =
                                CompletionResponse::Array(members.chain(methods).collect());
                            let result = serde_json::to_value(&result)?;
                            let resp = lsp::Response {
                                id,
                                result: Some(result),
                                error: None,
                            };
                            self.connection.sender.send(lsp::Message::Response(resp))?;
                            return Ok(());
                        }
                    }
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
                .insert(file_changed.clone(), FileId(self.file_ids.len(), mod_name));
            INPUT_PATH.write().unwrap().insert(
                self.file_ids[&file_changed],
                file_changed.to_file_path().unwrap(),
            );

            // If this was the first file in this session, look for other neighboring bytec files in the file system
            // We add them to the hashmaps with their url, so when they're opened in the editor it will automatically override them
            if self.file_ids.len() == 1 {
                let path = file_changed.to_file_path().unwrap().canonicalize().unwrap();
                let folder = path.parent().unwrap();
                for i in folder.read_dir().unwrap() {
                    let i = i.unwrap();
                    if i.file_name().to_str().unwrap().ends_with(".bt") {
                        let path = i.path();
                        let url = Url::from_file_path(&path).unwrap();
                        if url == file_changed {
                            continue;
                        }

                        let mod_name = self
                            .bindings
                            .raw(path.file_stem().unwrap().to_string_lossy());
                        let file = FileId(self.file_ids.len(), mod_name);
                        eprintln!("Adding neighboring file {}", url);

                        self.file_ids.insert(url.clone(), file);
                        self.source.insert(
                            url.clone(),
                            Rope::from_reader(std::fs::File::open(&path).unwrap()).unwrap(),
                        );
                        INPUT_PATH.write().unwrap().insert(file, path);
                        INPUT_SOURCE
                            .write()
                            .unwrap()
                            .insert(file, self.source[&url].clone());

                        // Parse it and add the items, but skip errors - we don't need errors in other files
                        let mut parser =
                            Parser::new(self.source[&url].slice(..), &mut self.bindings);
                        let (v, e) = parser.top_level();
                        self.parse_errors.insert(
                            file,
                            e.into_iter()
                                .map(|e| self.lsp_error(e, Severity::Error, &url))
                                .collect(),
                        );
                        self.parsed.insert(url, v);
                    }
                }
            }
        }

        INPUT_SOURCE.write().unwrap().insert(
            self.file_ids[&file_changed],
            self.source[&file_changed].clone(),
        );

        let mut parser = Parser::new(self.source[&file_changed].slice(..), &mut self.bindings);
        let (v, e) = parser.top_level();
        self.parsed.insert(file_changed.clone(), v);
        self.parse_errors.insert(
            self.file_ids[&file_changed],
            e.into_iter()
                .map(|e| self.lsp_error(e, Severity::Error, &file_changed))
                .collect(),
        );

        self.publish_diagnostics();
    }

    fn publish_diagnostics(&mut self) {
        let mut diagnostics = self.parse_errors.clone();
        let mut driver = Driver::new(
            self.parsed
                .iter()
                .map(|(k, v)| (self.file_ids[k], v.clone(), k.to_file_path().unwrap()))
                .collect(),
        );
        driver.run_all(&mut self.bindings);
        for (error, file) in driver.errors {
            diagnostics.entry(file).or_default().push(self.lsp_error(
                error,
                Severity::Error,
                self.file_ids.iter().find(|(_, v)| **v == file).unwrap().0,
            ));
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
        self.last_mods = driver.mods;
        self.last_mod_tys = driver.mods_pre.into_iter().collect();
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
