use std::fs;
use vulkan_parse::*;

#[test]
fn parse_vk_xml() {
    let xml = fs::read_to_string("Vulkan-Docs/xml/vk.xml").unwrap();
    let _registry = parse(&xml).unwrap();
}

#[test]
fn parse_video_xml() {
    let xml = fs::read_to_string("Vulkan-Docs/xml/video.xml").unwrap();
    let _registry = parse(&xml).unwrap();
}
