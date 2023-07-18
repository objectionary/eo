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
        let another_string = JObject::from(self.java_env.new_string(att).unwrap());
        self.java_env
           .call_method(&self.java_obj, "find", "(Ljava/lang/String;)I", &[JValue::from(&another_string)])
           .unwrap()
           .i()
           .unwrap()
    }
}
