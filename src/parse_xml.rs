use quick_xml::Reader;

use crate::Registry;

#[derive(Debug)]
pub enum Error {}

pub fn parse(xml: &str) -> Result<Registry, Error> {
    let reader = Reader::from_str(xml);
    todo!()
}
