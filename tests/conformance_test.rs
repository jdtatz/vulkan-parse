use ron::extensions::Extensions;
use ron::ser::{to_writer_pretty, PrettyConfig};
use std::fs;
use vulkan_parse::*;

fn write_registry(name: &str, reg: &Registry) {
    let config = PrettyConfig::default().extensions(Extensions::all());
    let f = fs::File::create(format!("{}.ron", name)).unwrap();
    to_writer_pretty(f, reg, config).unwrap();
}

#[test]
fn parse_vk_xml() {
    let xml = fs::read_to_string("Vulkan-Docs/xml/vk.xml").unwrap();
    let doc = XMLDocument::new(&xml).unwrap();
    let registry = match doc.parse() {
        Ok(reg) => reg,
        Err(e) => panic!("{}", e),
    };
    write_registry("vk", &registry)
}

#[test]
fn parse_video_xml() {
    let xml = fs::read_to_string("Vulkan-Docs/xml/video.xml").unwrap();
    let doc = XMLDocument::new(&xml).unwrap();
    let registry = match doc.parse() {
        Ok(reg) => reg,
        Err(e) => panic!("{}", e),
    };
    write_registry("video", &registry)
}
