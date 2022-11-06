use std::fs;

use serde_json::ser::to_writer_pretty;
use vulkan_parse::{parse_registry, Document};

#[test]
fn test_serialization() {
    let vk_xml = fs::read_to_string("Vulkan-Docs/xml/vk.xml").unwrap();
    let video_xml = fs::read_to_string("Vulkan-Docs/xml/video.xml").unwrap();

    let vk_doc = Document::parse(&vk_xml).unwrap();
    let video_doc = Document::parse(&video_xml).unwrap();

    let vk_registry = parse_registry(&vk_doc).unwrap();
    let video_registry = parse_registry(&video_doc).unwrap();

    let f = fs::File::create("vulkan.json").unwrap();
    to_writer_pretty(f, &[vk_registry, video_registry]).unwrap();
}
