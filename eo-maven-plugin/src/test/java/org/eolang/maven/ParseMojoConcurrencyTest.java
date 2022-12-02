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
    void parsesConcurrently(@TempDir final Path temp) {
        final Path foreign = new EoForeign(
            "eo-foreign.csv",
            IntStream.rangeClosed(1, ParseMojoConcurrencyTest.NUMBER_PROGRAMS).mapToObj(
                i -> new EoProgram("foo/x/main", i)
            )
        ).save(temp);
        final Path target = temp.resolve("target");
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("cache", temp.resolve("cache/parsed"))
            .with("foreignFormat", "csv")
            .execute();
        MatcherAssert.assertThat(this.allProgramsParsed(target), Matchers.is(true));
        MatcherAssert.assertThat(this.allTojosWrittenToFile(foreign), Matchers.is(true));
    }

    private boolean allProgramsParsed(final Path target) {
        return IntStream.rangeClosed(1, ParseMojoConcurrencyTest.NUMBER_PROGRAMS)
            .allMatch(
                i -> Files.exists(
                    target.resolve(
                        String.format("%s/foo/x/main/%d.%s", ParseMojo.DIR, i, TranspileMojo.EXT)
                    )
                )
            );
    }

    private boolean allTojosWrittenToFile(final Path foreign) {
        return IntStream.rangeClosed(1, ParseMojoConcurrencyTest.NUMBER_PROGRAMS)
            .allMatch(
                i -> new TjSmart(
                    Catalogs.INSTANCE.make(foreign)
                ).getById(String.format("foo.x.main.%d", i)).exists("xmir")
            );
    }

    /**
     * Eo foreign file, eo-foreign.json or eo-foreign.csv.
     *
     * @since 0.28.11
     */
    private static final class EoForeign {
        /**
         * File name, for example, eo-foreign.json.
         */
        private final String name;

        /**
         * All eo programs.
         */
        private final Stream<EoProgram> programs;

        /**
         * The main constructor.
         *
         * @param name EO foreign file name
         * @param programs EO programs
         */
        private EoForeign(
            final String name,
            final Stream<EoProgram> programs
        ) {
            this.name = name;
            this.programs = programs;
        }

        private Path save(final Path dir) {
            final Path path = dir.resolve(this.name);
            final Tojos tojos = Catalogs.INSTANCE.make(path);
            this.programs.forEach(
                program -> tojos.add(program.packageName())
                    .set(AssembleMojo.ATTR_SCOPE, "compile")
                    .set(AssembleMojo.ATTR_EO, program.save(dir).toString())
            );
            return path;
        }
    }

    /**
     * EO program.
     *
     * @since 0.28.11
     */
    private static final class EoProgram {

        /**
         * Eo program number in test.
         */
        private final int number;

        /**
         * Eo program name with path.
         */
        private final String name;

        /**
         * The main constructor.
         *
         * @param name Name of EO program
         * @param number EO program number
         */
        private EoProgram(
            final String name,
            final int number
        ) {
            this.name = name;
            this.number = number;
        }

        private String packageName() {
            return String.format(
                "%s.%d",
                this.name.replace("/", "."),
                this.number
            );
        }

        private Path save(final Path dir) {
            try {
                final Path src = dir.resolve(String.format("%s_%d.eo", this.name, this.number));
                new Home().save(this.programText(), src);
                return src;
            } catch (final IOException ex) {
                throw new IllegalStateException(
                    String.format("EoProgram #%d can't be saved into %s", this.number, dir),
                    ex
                );
            }
        }

        private String programText() {
            return "+package f\n\n[args] > main\n  (stdout \"Hello!\").print\n";
        }
    }
}