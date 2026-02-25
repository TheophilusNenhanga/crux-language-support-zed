use zed_extension_api::Extension;

struct CruxExtension;

impl Extension for CruxExtension {
    fn name() -> &'static str {
        "Crux"
    }
}
