/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

pub mod eo_enum;
use jni::JNIEnv;
use jni::objects::{JClass, JObject, JValue, JByteArray};

pub struct EOEnv<'local> {
    pub java_env: JNIEnv<'local>,
    _java_class: JClass<'local>,
    java_obj: JObject<'local>
}

/*
 * @todo #2442:45min Add correct processing of the return value of functions.
 *  call_method returns Result<JValueOwned<'local>> which we should not just unwrap.
 *  We need to check it instead and return None if exception in java side happened.
*/
impl<'local> EOEnv<'_> {
    pub fn new(java_env: JNIEnv<'local>, _java_class: JClass<'local>, java_obj: JObject<'local>) -> EOEnv<'local> {
        EOEnv {
            java_env,
            _java_class,
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
        let JavaVal =  self.java_env
            .call_method(
                &self.java_obj,
                "put",
                "(I[B)V",
                &[JValue::Int(v as i32), JValue::from(&jbytes)]
            );
        return if JavaVal.is_err() {
            self.java_env.exception_clear();
            None
        } else {
            JavaVal.unwrap().v().ok()
        }
    }

    pub fn bind(&mut self, v1: u32, v2: u32, name: &str) -> Option<()> {
        let java_val =  self.java_env
            .call_method(
                &self.java_obj,
                "bind",
                "(IILjava/lang/String;)V",
                &[
                    JValue::Int(v1 as i32),
                    JValue::Int(v2 as i32),
                    JValue::from(&JObject::from(self.java_env.new_string(name).unwrap()))
                ]
            );
        return if java_val.is_err() {
            self.java_env.exception_clear().ok()?;
            None
        } else {
            java_val.unwrap().v().ok()
        };
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

    pub fn dataize(&mut self, v: u32) -> Option<Vec<u8>> {
        let java_array = JByteArray::from(
            self.java_env
            .call_method(
                &self.java_obj,
                "dataize",
                "(I)[B",
                &[
                    JValue::Int(v as i32),
                ]
            ).unwrap().l().unwrap());
        let size = self.java_env.get_array_length(&java_array).unwrap();
        let mut bytes = vec![0; size.try_into().unwrap()];
        if self.java_env.get_byte_array_region(&java_array, 0, &mut bytes[0..]).is_err() {
            return None;
        }
        let unsigned = unsafe { &*(&bytes[0..] as *const _  as *const [u8]) };
        return Some(unsigned.to_vec());
    }
}
