use std::fs;

use serde_json::ser::to_writer_pretty;
use vulkan_parse::parse_registry;

#[test]
fn test_serialization() {
    let vk_xml = fs::read_to_string("Vulkan-Docs/xml/vk.xml").unwrap();
    let video_xml = fs::read_to_string("Vulkan-Docs/xml/video.xml").unwrap();

    let vk_registry = match parse_registry(&vk_xml) {
        Ok(reg) => reg,
        Err(e) => panic!("{}", e),
    };
    let video_registry = match parse_registry(&video_xml) {
        Ok(reg) => reg,
        Err(e) => panic!("{}", e),
    };

    let f = fs::File::create("vulkan.json").unwrap();
    to_writer_pretty(f, &[vk_registry, video_registry]).unwrap();
}
