/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link Inference}.
 * @since 0.67.0
 */
final class InferenceTest {

    @Test
    void splitsCompositeBase(@TempDir final Path temp) throws IOException {
        final Path input = Files.createDirectories(temp.resolve("chain"));
        Files.writeString(
            input.resolve("inc.xmir"),
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[x] > inc",
                    "  x.next.foo > @",
                    ""
                )
            ).parsed().toString()
        );
        new Inference(input).inferTo(temp.resolve("out"), temp.resolve("rows"));
        MatcherAssert.assertThat(
            "a chain of dispatches must become one object per step, but it didnt",
            new XMLDocument(temp.resolve("out").resolve("inc.xmir")),
            XhtmlMatchers.hasXPath("//o[@base='.foo']/o[@base='.next']/o[@base='ξ.x']")
        );
    }

    @Test
    void keepsReferenceWhole(@TempDir final Path temp) throws IOException {
        final Path input = Files.createDirectories(temp.resolve("plumbing"));
        Files.writeString(
            input.resolve("tap.xmir"),
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[] > tap",
                    "  water > @",
                    "  [] > water",
                    ""
                )
            ).parsed().toString()
        );
        new Inference(input).inferTo(temp.resolve("ready"), temp.resolve("rows"));
        MatcherAssert.assertThat(
            "a reference takes no attribute from anything, so it must stay whole, but it didnt",
            new XMLDocument(temp.resolve("ready").resolve("tap.xmir")),
            XhtmlMatchers.hasXPath("//o[@base='ξ.water']")
        );
    }

    @Test
    void locatesObjectsBornFromSplitting(@TempDir final Path temp) throws IOException {
        final Path sources = Files.createDirectories(temp.resolve("deep"));
        Files.writeString(
            sources.resolve("box.xmir"),
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[x] > box",
                    "  x.lid.hinge > @",
                    ""
                )
            ).parsed().toString()
        );
        new Inference(sources).inferTo(temp.resolve("done"), temp.resolve("rows"));
        MatcherAssert.assertThat(
            "the receiver of a new dispatch must get a locator of its own, but it didnt",
            new XMLDocument(temp.resolve("done").resolve("box.xmir")),
            XhtmlMatchers.hasXPath("//o[@base='ξ.x' and @loc='Φ.box.φ.ρ.ρ']")
        );
    }

    @Test
    void buildsProvidesTableForWholeProgram(@TempDir final Path temp) throws IOException {
        final Path folder = Files.createDirectories(temp.resolve("program"));
        Files.writeString(
            folder.resolve("app.xmir"),
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[] > app",
                    "  inc t > @",
                    "  [] > t",
                    "    [] > next",
                    "  [x] > inc",
                    "    x.next.foo > @",
                    ""
                )
            ).parsed().toString()
        );
        new Inference(folder).inferTo(temp.resolve("xmir"), temp.resolve("tables"));
        MatcherAssert.assertThat(
            "the innermost formation must be known to have nothing, but it isnt",
            new XMLDocument(temp.resolve("tables").resolve("provides.xml")),
            XhtmlMatchers.hasXPath(
                "/provides/type[@id='Φ.app.t.next' and @complete='true' and not(attr)]"
            )
        );
    }

    @Test
    void putsEveryFileOfProgramInOneTable(@TempDir final Path temp) throws IOException {
        final Path many = Files.createDirectories(temp.resolve("many"));
        Files.writeString(
            many.resolve("kettle.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > kettle", "  [] > steam", "")
            ).parsed().toString()
        );
        Files.createDirectories(many.resolve("nested"));
        Files.writeString(
            many.resolve("nested").resolve("cup.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > cup", "  [] > handle", "")
            ).parsed().toString()
        );
        new Inference(many).inferTo(temp.resolve("all"), temp.resolve("tables"));
        MatcherAssert.assertThat(
            "one table must cover the whole program, but a file was left out of it",
            new XMLDocument(temp.resolve("tables").resolve("provides.xml")),
            XhtmlMatchers.hasXPaths(
                "/provides/type[@id='Φ.kettle']/attr[@name='steam']",
                "/provides/type[@id='Φ.cup']/attr[@name='handle']"
            )
        );
    }

    @Test
    void keepsFoldersOfProgram(@TempDir final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("tree").resolve("one")).resolve("leaf.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > leaf", "  [] > vein", "")
            ).parsed().toString()
        );
        new Inference(temp.resolve("tree")).inferTo(temp.resolve("copy"), temp.resolve("rows"));
        MatcherAssert.assertThat(
            "a file in a folder must be written to the same folder again, but it wasnt",
            Files.exists(temp.resolve("copy").resolve("one").resolve("leaf.xmir")),
            Matchers.is(true)
        );
    }
}
