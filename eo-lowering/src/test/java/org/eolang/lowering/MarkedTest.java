/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.w3c.dom.Element;

/**
 * Test case for {@link Marked}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class MarkedTest {

    @Test
    void turnsWholeBodyIntoAtomOfTheFragment(@Mktmp final Path temp) throws IOException {
        final Element fragment = MarkedTest.fragment(
            MarkedTest.number("name='φ'", "S3")
        );
        new Marked(fragment, MarkedTest.sum(temp), temp).apply();
        MatcherAssert.assertThat(
            "the fragment itself must become the atom, typed and pure, but it didnt",
            new Xml(fragment).text(),
            Matchers.allOf(
                Matchers.matchesPattern("(?s).* lowered=\"[0-9a-f]{12}\".*"),
                Matchers.containsString("pure=\"true\""),
                Matchers.containsString("<o base=\"∅\" loc=\"Φ.foo.f.a\" name=\"a\" type=\"Φ.number\"/>"),
                Matchers.not(Matchers.containsString("name=\"φ\"")),
                Matchers.endsWith("<o atom=\"Φ.number\" name=\"λ\"/></o>")
            )
        );
    }

    @Test
    void readsOwnVoidsInTheAtomOfTheFragment(@Mktmp final Path temp) throws IOException {
        new Marked(
            MarkedTest.fragment(MarkedTest.number("name='φ'", "S3")),
            MarkedTest.sum(temp), temp
        ).apply();
        MatcherAssert.assertThat(
            "the sidecar must read the voids of the fragment by their names, but it doesnt",
            MarkedTest.sidecar(temp),
            Matchers.containsString("new Dataized(this.take(\"b\")).asNumber();")
        );
    }

    @Test
    void keepsFragmentWithHelperBesideSiblingAtom(@Mktmp final Path temp) throws IOException {
        final Element fragment = MarkedTest.fragment(
            String.format(
                "<o base='ξ.a' loc='Φ.foo.f.h' name='h'/>%s",
                MarkedTest.number("name='φ'", "S3")
            )
        );
        new Marked(fragment, MarkedTest.sum(temp), temp).apply();
        MatcherAssert.assertThat(
            "a fragment binding a helper beside its body cannot become the atom itself, but it did",
            fragment.hasAttribute("lowered"),
            Matchers.is(false)
        );
    }

    @Test
    void outlinesMarkerInsideApplication(@Mktmp final Path temp) throws IOException {
        final Element fragment = MarkedTest.fragment(
            String.format(
                "<o base='Φ.bar' name='φ'>%s</o>", MarkedTest.number("as='α0'", "S3")
            )
        );
        new Marked(fragment, MarkedTest.sum(temp), temp).apply();
        MatcherAssert.assertThat(
            "the marker must become a call of a sibling atom over the voids, but it didnt",
            new Xml(fragment).text(),
            Matchers.matchesPattern(
                String.join(
                    "",
                    "(?s).*<o base=\"Φ.bar\" name=\"φ\"><o as=\"α0\" base=\"ξ.l🌵([0-9a-f]{12})\">",
                    "<o as=\"α0\" base=\"ξ.a\"/><o as=\"α1\" base=\"ξ.b\"/></o></o>",
                    "<o loc=\"Φ.foo.f.l🌵\\1\" lowered=\"\\1\" name=\"l🌵\\1\" pure=\"true\">",
                    "<o base=\"∅\" loc=\"Φ.foo.f.l🌵\\1.v0\" name=\"v0\" type=\"Φ.number\"/>",
                    "<o base=\"∅\" loc=\"Φ.foo.f.l🌵\\1.v1\" name=\"v1\" type=\"Φ.number\"/>",
                    "<o atom=\"Φ.number\" name=\"λ\"/></o></o>.*"
                )
            )
        );
    }

    @Test
    void climbsOverAnonymousFormation(@Mktmp final Path temp) throws IOException {
        final Element fragment = MarkedTest.fragment(
            String.format(
                "<o base='Φ.bar' name='φ'><o as='α0'>%s</o></o>",
                MarkedTest.number("name='q'", "S3")
            )
        );
        new Marked(fragment, MarkedTest.sum(temp), temp).apply();
        MatcherAssert.assertThat(
            "a marker inside an anonymous formation must reach the atom and the voids through ρ, but it doesnt",
            new Xml(fragment).text(),
            Matchers.matchesPattern(
                "(?s).*<o base=\"ξ.ρ.l🌵[0-9a-f]{12}\" name=\"q\"><o as=\"α0\" base=\"ξ.ρ.a\"/>.*"
            )
        );
    }

    @Test
    void replacesVoidMarkerWithReference(@Mktmp final Path temp) throws IOException {
        final Element fragment = MarkedTest.fragment(
            String.format(
                "<o base='Φ.bar' name='φ'>%s</o>", MarkedTest.number("as='α0'", "S1")
            )
        );
        new Marked(fragment, MarkedTest.sum(temp), temp).apply();
        MatcherAssert.assertThat(
            "a marker of a void must become a reference to it, but it didnt",
            new Xml(fragment).text(),
            Matchers.containsString("<o base=\"Φ.bar\" name=\"φ\"><o as=\"α0\" base=\"ξ.a\"/></o>")
        );
    }

    @Test
    void countsNoAtomForReference(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a reference is no atom, but it was counted as one",
            new Marked(
                MarkedTest.fragment(MarkedTest.number("name='φ'", "S1")),
                MarkedTest.sum(temp), temp
            ).apply(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void reachesVoidOfParentThroughRho(@Mktmp final Path temp) throws IOException {
        final Element fragment = MarkedTest.fragment(
            MarkedTest.number("name='φ'", "S3")
        );
        new Marked(
            fragment,
            MarkedTest.table(
                temp, "S1\tnumber\tvoid\tρ.n", "S2\tnumber\tvoid\ta",
                "S3\tnumber\tL_number_plus\tsym:S1\tsym:S2"
            ),
            temp
        ).apply();
        MatcherAssert.assertThat(
            "a body reading a void of the parent must call a sibling atom with ξ.ρ.n, but it doesnt",
            new Xml(fragment).text(),
            Matchers.allOf(
                Matchers.containsString("<o as=\"α0\" base=\"ξ.ρ.n\"/><o as=\"α1\" base=\"ξ.a\"/>"),
                Matchers.not(Matchers.containsString("name=\"f\" lowered"))
            )
        );
    }

    @Test
    void bindsLexicalBoxFromCommonAncestor(@Mktmp final Path temp) throws IOException {
        final Element fragment = MarkedTest.fragment(
            MarkedTest.number("name='φ'", "S2")
        );
        new Marked(
            fragment,
            MarkedTest.table(temp, "S1\tnumber\tvoid\ta", "S2\tnumber\tbox\tΦ.foo.g\ty=sym:S1"),
            temp
        ).apply();
        MatcherAssert.assertThat(
            "a box beside the fragment must be handed over as ξ.ρ.g, but it isnt",
            new Xml(fragment).text(),
            Matchers.containsString("<o as=\"α0\" base=\"ξ.ρ.g\"/><o as=\"α1\" base=\"ξ.a\"/>")
        );
    }

    @Test
    void bindsForeignBoxByItsLocator(@Mktmp final Path temp) throws IOException {
        final Element fragment = MarkedTest.fragment(
            MarkedTest.number("name='φ'", "S2")
        );
        new Marked(
            fragment,
            MarkedTest.table(temp, "S1\tnumber\tvoid\ta", "S2\tnumber\tbox\tΦ.bar.g\ty=sym:S1"),
            temp
        ).apply();
        MatcherAssert.assertThat(
            "a box outside the top-level object must be handed over by its locator, but it isnt",
            new Xml(fragment).text(),
            Matchers.containsString("<o as=\"α0\" base=\"Φ.bar.g\"/>")
        );
    }

    @Test
    void leavesEntryImpure(@Mktmp final Path temp) throws IOException {
        final Element fragment = MarkedTest.fragment(
            MarkedTest.number("name='φ'", "S2")
        );
        new Marked(
            fragment,
            MarkedTest.table(
                temp, "S1\tnumber\tvoid\ta",
                "S2\tnumber\tbox\tΦ.number.min\tρ=sym:S1\tx=number:40-14-00-00-00-00-00-00"
            ),
            temp
        ).apply();
        MatcherAssert.assertThat(
            "an atom entering a formation is not pure, but it was marked so",
            new Xml(fragment).text(),
            Matchers.not(Matchers.containsString("pure"))
        );
    }

    @Test
    void turnsBoolMarkerIntoAtom(@Mktmp final Path temp) throws IOException {
        final Element fragment = MarkedTest.fragment(
            String.join(
                "",
                "<o base='Φ.bar' name='φ'><o as='α0' base='Φ.bool'><o as='if'>",
                "<o base='∅' name='left'/><o base='∅' name='right'/>",
                "<o name='guard'><o name='λ'>S2</o></o><o name='λ'>L_fork</o></o></o></o>"
            )
        );
        new Marked(
            fragment,
            MarkedTest.table(
                temp, "S1\tnumber\tvoid\ta",
                "S2\tbool\tL_number_gt\tsym:S1\tnumber:00-00-00-00-00-00-00-00"
            ),
            temp
        ).apply();
        MatcherAssert.assertThat(
            "the whole bool marker must become one call of a bool atom, but it didnt",
            new Xml(fragment).text(),
            Matchers.allOf(
                Matchers.matchesPattern(
                    "(?s).*<o base=\"Φ.bar\" name=\"φ\"><o as=\"α0\" base=\"ξ.l🌵[0-9a-f]{12}\"><o as=\"α0\" base=\"ξ.a\"/></o></o>.*"
                ),
                Matchers.containsString("<o atom=\"Φ.bool\" name=\"λ\"/>")
            )
        );
    }

    @Test
    void referencesTupleVoidAsAWhole(@Mktmp final Path temp) throws IOException {
        final Element fragment = MarkedTest.fragment(
            String.join(
                "",
                "<o base='Φ.tuple' name='φ'>",
                MarkedTest.number("as='length'", "S2"),
                "<o as='head'><o name='λ'>S3</o></o><o as='tail'><o name='λ'>S4</o></o></o>"
            )
        );
        new Marked(
            fragment,
            MarkedTest.table(
                temp, "S1\ttuple\tvoid\tt", "S2\tnumber\tattr\tsym:S1\tlength",
                "S3\tobject\tattr\tsym:S1\thead", "S4\ttuple\tattr\tsym:S1\ttail"
            ),
            temp
        ).apply();
        MatcherAssert.assertThat(
            "the three parts of a tuple void must become one reference to it, but they didnt",
            new Xml(fragment).text(),
            Matchers.endsWith("<o base=\"ξ.t\" name=\"φ\"/></o>")
        );
    }

    @Test
    void outlinesWholeBodyOfTopLevelFormation(@Mktmp final Path temp) throws IOException {
        final Element fragment = (Element) new Xnav(
            String.format(
                "<object><o loc='Φ.f' name='f'><o base='∅' loc='Φ.f.a' name='a'/><o base='∅' loc='Φ.f.b' name='b'/>%s</o></object>",
                MarkedTest.number("name='φ'", "S3")
            )
        ).element("object").element("o").node();
        new Marked(fragment, MarkedTest.sum(temp), temp).apply();
        MatcherAssert.assertThat(
            "a top-level formation must call a sibling atom instead of becoming one, but it did",
            new Xml(fragment).text(),
            Matchers.matchesPattern("(?s).*<o base=\"ξ.l🌵[0-9a-f]{12}\" name=\"φ\">.*")
        );
    }

    @Test
    void refusesProgramJavaCannotRender(@Mktmp final Path temp) throws IOException {
        final Marked marked = new Marked(
            MarkedTest.fragment("<o name='φ'><o name='λ'>S2</o></o>"),
            MarkedTest.table(
                temp, "S1\ttuple\tvoid\tt",
                "S2\tobject\tL_tuple_at\tsym:S1\tnumber:00-00-00-00-00-00-00-00"
            ),
            temp
        );
        Assertions.assertThrows(
            IllegalStateException.class,
            marked::apply,
            "a program answering an object cannot be an atom, but it became one"
        );
    }

    private static String number(final String attr, final String sym) {
        return String.format(
            "<o %s base='Φ.number'><o as='φ' base='Φ.bytes'><o as='φ'><o name='λ'>%s</o></o></o></o>",
            attr, sym
        );
    }

    private static Table sum(final Path temp) throws IOException {
        return MarkedTest.table(
            temp,
            "S1\tnumber\tvoid\ta", "S2\tnumber\tvoid\tb",
            "S3\tnumber\tL_number_plus\tsym:S1\tsym:S2"
        );
    }

    private static Element fragment(final String body) {
        return new Located(
            (Element) new Xnav(
                String.format(
                    String.join(
                        "",
                        "<object><o loc='Φ.foo' name='foo'><o base='∅' loc='Φ.foo.n' name='n'/>",
                        "<o loc='Φ.foo.f' name='f'><o base='∅' loc='Φ.foo.f.a' name='a'/>",
                        "<o base='∅' loc='Φ.foo.f.b' name='b'/>%s</o></o></object>"
                    ),
                    body
                )
            ).element("object").node(),
            "Φ.foo.f"
        ).element();
    }

    private static Table table(final Path temp, final String... rows) throws IOException {
        final Path file = temp.resolve("symbols.tsv");
        Files.write(
            file, String.join("\n", rows).concat("\n").getBytes(StandardCharsets.UTF_8)
        );
        return new Table(new Symbols(file));
    }

    private static String sidecar(final Path temp) throws IOException {
        try (Stream<Path> files = Files.list(temp)) {
            return Files.readString(
                files.filter(file -> file.toString().endsWith(".java")).findFirst().get(),
                StandardCharsets.UTF_8
            );
        }
    }


    @Test
    void sharesOneAtomBetweenSitesOfOneProgram(@Mktmp final Path temp) throws IOException {
        final Element fragment = MarkedTest.fragment(
            String.format(
                "%s%s", MarkedTest.number("name='h'", "S3"), MarkedTest.number("name='φ'", "S3")
            )
        );
        new Marked(fragment, MarkedTest.sum(temp), temp).apply();
        MatcherAssert.assertThat(
            "two sites of one program must call one sibling atom, but two were made",
            new Xnav(fragment).path("o[starts-with(@name, 'l🌵')]").count(),
            Matchers.equalTo(1L)
        );
    }
}
