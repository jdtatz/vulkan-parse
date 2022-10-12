use std::fs;
use vulkan_parse::*;

#[test]
fn parse_vk_xml() {
    let xml = fs::read_to_string("Vulkan-Docs/xml/vk.xml").unwrap();
    let doc = XMLDocument::new(&xml).unwrap();
    let _registry = match doc.parse() {
        Ok(reg) => reg,
        Err(e) => panic!("{}", e),
    };
}

#[test]
fn parse_video_xml() {
    let xml = fs::read_to_string("Vulkan-Docs/xml/video.xml").unwrap();
    let doc = XMLDocument::new(&xml).unwrap();
    let _registry = doc.parse().unwrap();
}
