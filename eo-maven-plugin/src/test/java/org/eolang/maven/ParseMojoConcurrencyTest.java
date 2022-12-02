/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
package org.eolang.maven;

import com.yegor256.tojos.TjSmart;
import com.yegor256.tojos.Tojos;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Concurrency test for ParseMojo.
 *
 * @since 0.28.11
 */
class ParseMojoConcurrencyTest {

    /**
     * Number of programs to parse.
     */
    private static final int NUMBER_PROGRAMS = 50;

    @Test
    void parsesConcurrentlyWithNewApi(@TempDir final Path temp) throws IOException {
        FakeMaven maven = new FakeMaven(temp);
        for (int i = 0; i < NUMBER_PROGRAMS; i++) {
            maven.withProgram("+package f\n\n[args] > main\n  (stdout \"Hello!\").print\n");
        }
        maven.execute(ParseMojo.class);
        MatcherAssert.assertThat(this.allProgramsParsed(maven.targetPath()), Matchers.is(true));
        MatcherAssert.assertThat(this.allTojosWrittenToFile(maven.foreignPath()), Matchers.is(true));
    }

    private boolean allProgramsParsed(final Path target) {
        return IntStream.range(0, ParseMojoConcurrencyTest.NUMBER_PROGRAMS)
            .allMatch(
                i -> Files.exists(
                    target.resolve(
                        String.format("%s/foo/x/main%s.%s", ParseMojo.DIR, suffix(i), TranspileMojo.EXT)
                    )
                )
            );
    }

    private String suffix(int i){
        if(i == 0){
            return "";
        } else {
            return String.format("_%d", i);
        }
    }

    private boolean allTojosWrittenToFile(final Path foreign) {
        return IntStream.range(0, ParseMojoConcurrencyTest.NUMBER_PROGRAMS)
            .allMatch(
                i -> new TjSmart(
                    Catalogs.INSTANCE.make(foreign)
                ).getById(String.format("foo.x.main%s", suffix(i))).exists("xmir")
            );
    }

}