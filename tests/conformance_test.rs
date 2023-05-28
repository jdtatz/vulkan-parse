use std::{collections::HashSet, fs, io::Cursor};

use roxmltree::{Document, Node};
use vulkan_parse::{parse_registry, roundtrip::into_xml, tokenize, Expression};

#[test]
fn test_vk_xml_conformance() {
    let xml = fs::read_to_string("Vulkan-Docs/xml/vk.xml").unwrap();
    let registry = match parse_registry(&xml) {
        Ok(reg) => reg,
        Err(e) => panic!("{}", e),
    };
    let mut roundtrip_xml = Vec::new();
    into_xml(&registry, Cursor::new(&mut roundtrip_xml)).unwrap();
    fs::write("vk.roundtrip.xml", &roundtrip_xml).unwrap();
    let roundtrip_xml = String::from_utf8(roundtrip_xml).unwrap();
    xml_compare(&xml, &roundtrip_xml, "Vulkan-Docs/xml/vk.xml");
}

#[test]
fn test_video_xml_conformance() {
    let xml = fs::read_to_string("Vulkan-Docs/xml/video.xml").unwrap();
    let registry = match parse_registry(&xml) {
        Ok(reg) => reg,
        Err(e) => panic!("{}", e),
    };
    let mut roundtrip_xml = Vec::new();
    into_xml(&registry, Cursor::new(&mut roundtrip_xml)).unwrap();
    fs::write("video.roundtrip.xml", &roundtrip_xml).unwrap();
    let roundtrip_xml = String::from_utf8(roundtrip_xml).unwrap();
    xml_compare(&xml, &roundtrip_xml, "Vulkan-Docs/xml/video.xml");
}

const UNORDERED_ATTRS: &[&str] = &["queues", "cmdbufferlevel", "tasks", "api"];

fn xml_compare(standard_xml: &str, roundtrip_xml: &str, path: &str) {
    let standard_xml = standard_xml.replace("__IOSurface*", "<type>__IOSurface</type>*");
    let standard_doc = Document::parse(&standard_xml).unwrap();
    let roundtrip_doc = Document::parse(roundtrip_xml).unwrap();

    // ignore comment nodes and empty text nodes
    let node_filter =
        |n: &Node| !n.is_comment() && (!n.is_text() || n.text().map_or(false, |s| s.trim() != ""));
    let standard_nodes = standard_doc.descendants().filter(node_filter);
    let roundtrip_nodes = roundtrip_doc.descendants().filter(node_filter);
    for (s, r) in standard_nodes.zip(roundtrip_nodes) {
        if s.is_element() {
            if !r.is_element()
                || (s.tag_name() != r.tag_name())
                || (s.attributes().len() != r.attributes().len())
            {
                // panic!("standard {s:#?} != roundtrip {r:#?} at {path}:{}", standard_doc.text_pos_at(s.range().start));
                assert_eq!(s, r, "{path}:{}", standard_doc.text_pos_at(s.range().start));
            }
            for attr in s.attributes() {
                let r_attr_val = if let Some(r_attr_val) = r.attribute(attr.name()) {
                    r_attr_val
                } else if attr.name() == "requires" {
                    // 'bitvalues' implies a stronger relationship then 'requires' which only implies a dependency
                    // but in the generator there is nothing diffrent between the 2
                    // see Vulkan-Docs/scripts/reg.py:1251:
                    r.attribute("bitvalues").unwrap()
                } else {
                    todo!()
                };
                if attr.value() != r_attr_val {
                    if UNORDERED_ATTRS.contains(&attr.name()) {
                        let s_set = attr.value().split(',').collect::<HashSet<_>>();
                        let r_set = r_attr_val.split(',').collect::<HashSet<_>>();
                        assert_eq!(
                            s_set,
                            r_set,
                            "{path}:{}",
                            standard_doc.text_pos_at(s.range().start)
                        );
                    } else if let Some(s_ver) = attr.value().strip_prefix("VK_API_VERSION_") {
                        // # VK_VERSION_* are the guard macros and VK_API_VERSION_* are the version number macros
                        // # see Vulkan-Docs/scripts/spirvcapgenerator.py:125:
                        assert_eq!(s_ver, r_attr_val.strip_prefix("VK_VERSION_").unwrap());
                    } else {
                        let s_expr = Expression::try_from(attr.value()).unwrap();
                        let r_expr = Expression::try_from(r_attr_val).unwrap();
                        assert_eq!(s_expr, r_expr)
                    }
                }
            }
        } else if s.is_text() {
            let std_txt = s.text().unwrap();
            let rt_txt = r.text().unwrap();
            if std_txt != rt_txt {
                let std_toks = tokenize(std_txt, false, true)
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let rt_toks: Vec<_> = tokenize(rt_txt, false, true)
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                assert_eq!(
                    std_toks,
                    rt_toks,
                    "{path}:{}",
                    standard_doc.text_pos_at(s.range().start)
                );
            }
        }
    }
}
