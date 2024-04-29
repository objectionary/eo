/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
package org.eolang.maven.rust;

/**
 * Prime Module in the rust project. Contains the function to call from the java side.
 * @since 0.29.7
 */
public class PrimeModule extends Module {

    /**
     * Ctor.
     *
     * @param method How the function is named from the java side.
     * @param file File name.
     */
    public PrimeModule(final String method, final String file) {
        super(
            String.join(
                System.lineSeparator(),
                "mod foo;",
                "use foo::foo;",
                "use jni::JNIEnv;",
                "use jni::objects::{JByteArray, JClass, JObject};",
                "use eo::Portal;",
                "use eo::eo_enum::EO::EOError;",
                "#[no_mangle]",
                "pub extern \"system\" fn",
                translate(method),
                "<'local> (env: JNIEnv<'local>, _class: JClass<'local>, universe: JObject<'local>) -> JByteArray<'local>",
                "{ let mut eo = Portal::new(env, _class, universe); ",
                "let arr = foo(&mut eo)",
                ".unwrap_or_else(||EOError(\"Rust function returned None\".to_string()))",
                ".eo2vec();",
                "eo.java_env.byte_array_from_slice(&arr.as_slice()).unwrap() }"
            ),
            file
        );
    }

    /**
     * Translates ("mangles") Java method name to native method name.
     * For example, translated "native0_custom" to
     * Java_EOrust_natives_native0_1custom_native0_1custom.
     * See {@link PrimeModule#translate(String, String)} for motivation.
     * @param jname Name of this function (and class) in Java.
     * @return How it named in Rust.
     */
    private static String translate(final String jname) {
        return PrimeModule.translate(
            "EOrust.natives.".concat(jname),
            jname
        );
    }

    /**
     * Translates ("mangles") Java method name to native method name according to
     * <a href="https://docs.oracle.com/javase/1.5.0/docs/guide/jni/spec/design.html">documentation</a>
     * or <a href="https://stackoverflow.com/questions/32470463/what-is-the-naming-convention-for-java-native-interface-method-and-module-name">stackoverflow</a>.
     * @param clazz Simple name of class.
     * @param method Name of method.
     * @return How it named in Rust.
     */
    private static String translate(final String clazz, final String method) {
        return String.format(
            "Java_%s_%s",
            clazz.replace("_", "_1").replace(".", "_"),
            method.replace("_", "_1")
        );
    }
}
