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
package org.eolang.maven.rust;

import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.text.TextOf;
import org.eolang.maven.footprint.FtDefault;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link Module}.
 *
 * @since 0.1
 */
final class ModuleTest {
    @Test
    void transformsCorrectly(@TempDir final Path temp) throws Exception {
        final Module module = new Module(
            String.join(
                System.lineSeparator(),
                "pub fn foo() -> i32 {",
                "  let mut rng = rand::thread_rng();",
                "  print!(\"Hello world\");",
                "  let i = rng.gen::<i32>();",
                "  i",
                "}"
            ),
            "simple"
        );
        module.save(new FtDefault(temp.resolve(Paths.get("qwerty"))));
        MatcherAssert.assertThat(
            new TextOf(
                temp.resolve(Paths.get("qwerty").resolve("src").resolve("simple.rs"))
            ).asString(),
            Matchers.stringContainsInOrder(
                "use jni::objects::{JClass};",
                "use jni::sys::{jint};",
                "use jni::JNIEnv;",
                "#[no_mangle]",
                "pub extern \"system\" fn Java_EOrust_natives_simple_simple(_env: JNIEnv, _class: JClass,) -> jint {"
            )
        );
    }
}
