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

package org.eolang.maven.hash;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.io.ResourceOf;
import org.eolang.maven.OnlineCondition;
import org.eolang.maven.util.Home;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link ChCompound}.
 * @since 0.28.14
 */
final class ChCompoundTest {

    @Test
    @ExtendWith(OnlineCondition.class)
    void getsCommitHashValueFromRemoteTag() {
        MatcherAssert.assertThat(
            new ChCompound(
                null,
                null,
                "0.26.0"
            ).value(),
            Matchers.equalTo("e0b783692ef749bb184244acb2401f551388a328")
        );
    }

    @Test
    void getsCommitHashValueFromPattern() {
        MatcherAssert.assertThat(
            new ChCompound(
                null,
                "master:m23ss3h,3.1.*:abc2sd3",
                "master"
            ).value(),
            Matchers.equalTo("m23ss3h")
        );
    }

    @Test
    void getsCommitHashValueFromFile(@TempDir final Path temp) throws IOException {
        final Path file = temp.resolve("tags.txt");
        new Home(temp).save(
            new ResourceOf("org/eolang/maven/commits/tags.txt"),
            Paths.get("tags.txt")
        );
        MatcherAssert.assertThat(
            new ChCompound(
                file,
                null,
                "master"
            ).value(),
            Matchers.equalTo("mmmmmmm807fae45ab3ef497451b1066bdd7704c5")
        );
    }

    @Test
    @ExtendWith(OnlineCondition.class)
    void catchesAnExceptionWhenNoArguments() {
        Assertions.assertThrows(
            NullPointerException.class,
            () -> new ChCompound(null, null, null).value()
        );
    }

}
