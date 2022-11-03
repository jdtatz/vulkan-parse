use std::{collections::HashSet, fs, io::Cursor};

use ron::{
    extensions::Extensions,
    ser::{to_writer_pretty, PrettyConfig},
};
use roxmltree::Node;
use vulkan_parse::{into_xml::into_xml, parse_registry, tokenize, Document, Expression, Registry};

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
    let mut roundtrip_xml = Vec::new();
    into_xml(&registry, Cursor::new(&mut roundtrip_xml)).unwrap();
    fs::write("vk.roundtrip.xml", &roundtrip_xml).unwrap();
    let roundtrip_xml = String::from_utf8(roundtrip_xml).unwrap();
    // commented out for now b/c it takes 20 seconds
    // write_registry("vk", &registry);
    xml_compare(&xml, &roundtrip_xml);
}

#[test]
fn parse_video_xml() {
    let xml = fs::read_to_string("Vulkan-Docs/xml/video.xml").unwrap();
    let doc = Document::parse(&xml).unwrap();
    let registry = match parse_registry(&doc) {
        Ok(reg) => reg,
        Err(e) => panic!("{}", e),
    };
    let mut roundtrip_xml = Vec::new();
    into_xml(&registry, Cursor::new(&mut roundtrip_xml)).unwrap();
    fs::write("video.roundtrip.xml", &roundtrip_xml).unwrap();
    let roundtrip_xml = String::from_utf8(roundtrip_xml).unwrap();
    xml_compare(&xml, &roundtrip_xml);
    write_registry("video", &registry);
}

const UNORDERED_ATTRS: &[&str] = &["queues", "cmdbufferlevel", "tasks"];

fn xml_compare(standard_xml: &str, roundtrip_xml: &str) {
    let standard_xml = standard_xml.replace("__IOSurface*", "<type>__IOSurface</type>*");
    let standard_doc = Document::parse(&standard_xml).unwrap();
    let roundtrip_doc = Document::parse(&roundtrip_xml).unwrap();

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
                panic!("standard {:#?} != roundtrip {:#?}", s, r);
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
                        assert_eq!(s_set, r_set);
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
                assert!(tokenize(std_txt, false, true).eq(tokenize(rt_txt, false, true)));
            }
        }
    }
}
