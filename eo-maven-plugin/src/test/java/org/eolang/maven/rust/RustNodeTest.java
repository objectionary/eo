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

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link RustNode}.
 * @since 0.34
 */
@ExtendWith(MktmpResolver.class)
final class RustNodeTest {

    @Test
    void generatesRust(@Mktmp final Path temp) throws IOException {
        final XML insert = new XMLDocument(
            "<rust code=\"75-73-65-20\" code_loc=\"Φ.org.eolang.custom-rust.r.α0\"><dependencies/></rust>"
        ).nodes("rust").get(0);
        final Path lib = temp.resolve("Lib");
        final Path  gen = temp.resolve("generated");
        new RustNode(
            insert,
            new Names(temp.resolve("names")),
            lib,
            Paths.get("../eo-runtime/src/main/rust/eo"),
            gen
        ).generate();
        final List<Path> out = Files.walk(temp).collect(Collectors.toList());
        MatcherAssert.assertThat(
            "Check that necessary files are created",
            out,
            Matchers.hasItems(
                lib.resolve("native0_QQ_custom_rust_r_0/src/foo.rs"),
                lib.resolve("native0_QQ_custom_rust_r_0/src/lib.rs"),
                gen.resolve("native0_QQ_custom_rust_r_0.java")
            )
        );
    }
}
