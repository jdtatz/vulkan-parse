use std::fs;

use ron::{
    extensions::Extensions,
    ser::{to_writer_pretty, PrettyConfig},
};
use vulkan_parse::{parse_registry, Document, Registry};

fn write_registry(name: &str, reg: &Registry) {
    let config = PrettyConfig::default().extensions(Extensions::all());
    let f = fs::File::create(format!("{}.ron", name)).unwrap();
    to_writer_pretty(f, reg, config).unwrap();
}

#[test]
fn parse_vk_xml() {
    let xml = fs::read_to_string("Vulkan-Docs/xml/vk.xml").unwrap();
    let doc = Document::parse(&xml).unwrap();
    let registry = match parse_registry(&doc) {
        Ok(reg) => reg,
        Err(e) => panic!("{}", e),
    };
    write_registry("vk", &registry);
}

#[test]
fn parse_video_xml() {
    let xml = fs::read_to_string("Vulkan-Docs/xml/video.xml").unwrap();
    let doc = Document::parse(&xml).unwrap();
    let registry = match parse_registry(&doc) {
        Ok(reg) => reg,
        Err(e) => panic!("{}", e),
    };
    write_registry("video", &registry);
}
