use jni::JNIEnv;
use jni::objects::{JClass, JObject, JValue};

#[allow(dead_code)]
pub struct EOEnv<'local> {
    java_env: JNIEnv<'local>,
    java_class: JClass<'local>,
    java_obj: JObject<'local>
}

impl<'local> EOEnv<'_> {
    pub fn new(java_env: JNIEnv<'local>, java_class: JClass<'local>, java_obj: JObject<'local>) -> EOEnv<'local> {
        EOEnv {
            java_env,
            java_class,
            java_obj
        }
    }

    pub fn find(&mut self, att: &str) -> i32 {
        let jstr = JObject::from(self.java_env.new_string(att).unwrap());
        self.java_env
           .call_method(&self.java_obj, "find", "(Ljava/lang/String;)I", &[JValue::from(&jstr)])
           .unwrap()
           .i()
           .unwrap()
    }

    pub fn put(&mut self, v: u32, bytes: &[u8]) -> Option<()> {
        let jbytes = JObject::from(self.java_env.byte_array_from_slice(bytes).unwrap());
        match self.java_env
            .call_method(
                &self.java_obj,
                "put",
                "(I[B)V",
                &[JValue::Int(v as i32), JValue::from(&jbytes)]
            )
            .unwrap()
            .v() {
            Ok(()) => Some(()),
            _ => None
        }
    }

    pub fn bind(&mut self, v1: u32, v2: u32, name: &str) -> Option<()> {
        match self.java_env
            .call_method(
                &self.java_obj,
                "bind",
                "(IILjava/lang/String;)V",
                &[
                    JValue::Int(v1 as i32),
                    JValue::Int(v2 as i32),
                    JValue::from(&JObject::from(self.java_env.new_string(name).unwrap()))
                ]
            ).unwrap().v() {
            Ok(()) => Some(()),
            _ => None,
        }
    }

    pub fn copy(&mut self, v: u32) -> Option<u32> {
        match self.java_env
            .call_method(
                &self.java_obj,
                "copy",
                "(I)I",
                &[
                    JValue::Int(v as i32),
                ]
            ).unwrap().i() {
            Ok(ret) => Some(ret.try_into().unwrap()),
            _ => None,
        }
    }

    /*
     * @todo #2237:60min Implement "dataize" method. It must
     *  call java dataizing method and get byte array from it.
     *  Then it have to return byte array as a result of dataization.
     */
    pub fn dataize(&mut self, v: u32) -> Option<&[u32]> {
        Some(&[0x0])
    }
}
