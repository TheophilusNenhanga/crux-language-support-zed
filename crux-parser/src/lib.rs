use std::sync::OnceLock;
use zed_extension_api::{
    extension::Extension, worktree::Worktree, Command, LanguageServerId, Result,
};

static RUNTIME: OnceLock<tokio::runtime::Runtime> = OnceLock::new();

struct CruxExtension;

impl Extension for CruxExtension {
    fn name() -> &'static str {
        "Crux"
    }

    fn language_server_command(
        &mut self,
        language_server_id: &LanguageServerId,
        _worktree: &Worktree,
    ) -> Result<Command> {
        if *language_server_id != "crux-lsp" {
            return Err(format!("Unknown language server ID: {}", language_server_id).into());
        }

        let extension_root = std::env::current_exe()
            .ok()
            .and_then(|p| p.parent().map(|p| p.to_path_buf()))
            .ok_or("Failed to get extension root")?;

        let lsp_path = extension_root.join("crux-lsp");

        Ok(Command {
            command: lsp_path,
            args: vec![],
            env: std::collections::HashMap::new(),
        })
    }
}
