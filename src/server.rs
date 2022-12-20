use std::collections::HashMap;
use std::error::Error;
use std::hash::Hash;
use std::io::Read;

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
    bindings: Bindings,
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
            hover_provider: Some(HoverProviderCapability::Simple(true)),
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
            io_threads,
            connection,
            initialization_params,
            bindings: Default::default(),
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
            request::HoverRequest::METHOD => {
                let (id, params): (_, lsp_types::HoverParams) =
                    msg.extract(request::HoverRequest::METHOD)?;
                // let file = self.db.file_id(FileLoc::Url(
                //     params.text_document_position_params.text_document.uri,
                // ));
                // let pos = params.text_document_position_params.position;
                // let source = self.source.get(&file).unwrap();
                // let pos = source
                //     .char_to_byte(source.line_to_char(pos.line as usize) + pos.character as usize)
                //     as u32;
                // if let Some(split) = self.db.split_at(file, pos) {
                //     let aspan = self.db.split_span(file, split).unwrap();
                //     if let Some((ty, span)) = crate::elab::ide_support::hover_type(
                //         &self.db,
                //         file,
                //         split,
                //         pos - aspan.1.start,
                //     ) {
                //         let range = aspan.add(span).lsp_range(&self.source);
                //         let result = Hover {
                //             contents: HoverContents::Scalar(MarkedString::LanguageString(
                //                 LanguageString {
                //                     language: "pika".into(),
                //                     value: ty.to_string(false),
                //                 },
                //             )),
                //             range: Some(range),
                //         };
                //         let result = serde_json::to_value(&result)?;
                //         let resp = lsp::Response {
                //             id,
                //             result: Some(result),
                //             error: None,
                //         };
                //         self.connection.sender.send(lsp::Message::Response(resp))?;
                //         return Ok(());
                //     }
                // }
                // let resp = lsp::Response {
                //     id,
                //     result: Some(serde_json::Value::Null),
                //     error: None,
                // };
                // self.connection.sender.send(lsp::Message::Response(resp))?;
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
            let mod_name = self
                .bindings
                .raw(file_changed.path_segments().unwrap().nth_back(1).unwrap());
            self.file_ids
                .insert(file_changed.clone(), FileId(self.file_ids.len(), mod_name));
            INPUT_PATH.write().unwrap().insert(
                self.file_ids[&file_changed],
                file_changed.to_file_path().unwrap(),
            );
        }
        INPUT_SOURCE.write().unwrap().insert(
            self.file_ids[&file_changed],
            self.source[&file_changed].clone(),
        );

        let mut diagnostics = Vec::new();

        let mut defs = Default::default();
        let mut parser = Parser::new(
            self.source[&file_changed].slice(..),
            &mut self.bindings,
            &mut defs,
        );
        match parser.top_level() {
            Ok(v) => {
                self.parsed.insert(file_changed.clone(), v);
            }
            Err(e) => {
                diagnostics.push(self.lsp_error(e, Severity::Error, &file_changed));
            }
        }
        let mut driver = Driver::new(
            self.parsed
                .iter()
                .map(|(k, v)| (self.file_ids[k], v.clone(), k.to_file_path().unwrap()))
                .collect(),
        );
        driver.run_all(&mut self.bindings);
        for (error, file) in driver.errors {
            if file == self.file_ids[&file_changed] {
                diagnostics.push(self.lsp_error(error, Severity::Error, &file_changed));
            }
        }
        // TODO only send diagnostics if they changed
        let message = lsp::Notification {
            method: PublishDiagnostics::METHOD.to_string(),
            params: serde_json::to_value(&PublishDiagnosticsParams {
                uri: file_changed,
                version: None,
                diagnostics,
            })
            .unwrap(),
        };
        self.connection
            .sender
            .send(lsp::Message::Notification(message))
            .unwrap();
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
