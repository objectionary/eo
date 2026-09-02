/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link StLowered}, the {@code lowered.xsl} it runs, and
 * the atom call-site rendering of {@code to-java.xsl}.
 *
 * <p>The fixtures imitate what the {@code lower} goal will leave behind:
 * a formation whose body is replaced by a {@code λ} marker, stamped with
 * {@code @lowered} and {@code @pure}, and a sidecar file holding the body
 * of the {@code lambda()} method. The train below is the tail of the real
 * transpile train, so what these tests see is what a build sees.</p>
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class StLoweredTest {

    @Test
    void rendersLoweredFormationIntoItsOwnClass(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the train must append a class for the lowered formation, but it didnt",
            StLoweredTest.transpiled(
                StLoweredTest.lowered(StLoweredTest.program(), "3f9ab12cd45e"),
                StLoweredTest.sidecars(temp, "3f9ab12cd45e", "        return this.take(\"x\");")
            ).nodes("/object/class[@lowered='true' and @java-name='org.eolang.EOapp$EObump']/java"),
            Matchers.not(Matchers.empty())
        );
    }

    @Test
    void declaresGeneratedClassAsAtom(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the generated class must be a final PhDefault implementing Atom, but it isnt",
            StLoweredTest.generated(temp, "77aacd120e9b"),
            Matchers.containsString(
                "public final class EOapp$EObump extends PhDefault implements Atom {"
            )
        );
    }

    @Test
    void registersReceiverAndVoidInCtor(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the ctor must register the receiver and every void through super(), but it doesnt",
            StLoweredTest.generated(temp, "0b54e17d92cc"),
            Matchers.containsString(
                "super(new Attrs(new Attr(Phi.RHO, new AtRho()), new Attr(\"x\", new AtVoid(\"x\"))));"
            )
        );
    }

    @Test
    void splicesSidecarBodyVerbatim(@Mktmp final Path temp) throws IOException {
        final String body =
            "        return new Data.ToPhi(new Dataized(this.take(\"x\")).asNumber() * 7.0);";
        MatcherAssert.assertThat(
            "the sidecar body must land in lambda() verbatim, but it didnt",
            StLoweredTest.transpiled(
                StLoweredTest.lowered(StLoweredTest.program(), "8d3c11feab02"),
                StLoweredTest.sidecars(temp, "8d3c11feab02", body)
            ).xpath("/object/class[@lowered='true']/java/text()").get(0),
            Matchers.containsString(body)
        );
    }

    @Test
    void namesOriginalObjectInAnnotation(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the annotation must carry the original name of the formation, but it doesnt",
            StLoweredTest.generated(temp, "c19f04aa77d3"),
            Matchers.containsString("@XmirObject(oname = \"app.bump\")")
        );
    }

    @Test
    void callsGeneratedClassAtCallSite(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the call site must instantiate the generated class by name, but it doesnt",
            StLoweredTest.transpiled(
                StLoweredTest.lowered(StLoweredTest.program(), "5e5e207bd41a"),
                StLoweredTest.sidecars(temp, "5e5e207bd41a", "        return this.take(\"x\");")
            ).xpath("/object/class[@name='app']/java/text()").get(0),
            Matchers.containsString("new EOapp$EObump()")
        );
    }

    @Test
    void wrapsLoweredAtomIntoPhSticky(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the lowered atom must be wrapped into PhSticky at the call site, but it wasnt",
            StLoweredTest.transpiled(
                StLoweredTest.lowered(StLoweredTest.program(), "412906cafe33"),
                StLoweredTest.sidecars(temp, "412906cafe33", "        return this.take(\"x\");")
            ).xpath("/object/class[@name='app']/java/text()").get(0),
            Matchers.containsString("atom = new PhSticky(atom);")
        );
    }

    @Test
    void putsGeneratedClassIntoJavaPackage(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the generated class must live in the Java package of its program, but it doesnt",
            StLoweredTest.transpiled(
                StLoweredTest.lowered(
                    StLoweredTest.program("+package foo", ""), "b7d20ac54e19"
                ),
                StLoweredTest.sidecars(temp, "b7d20ac54e19", "        return this.take(\"x\");")
            ).nodes("/object/class[@java-name='org.eolang.EO_foo.EOapp$EObump']"),
            Matchers.not(Matchers.empty())
        );
    }

    @Test
    void leavesProgramWithoutLoweredFormationsAlone(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a program with nothing lowered must gain no extra classes, but it did",
            StLoweredTest.transpiled(
                StLoweredTest.program(), temp.resolve("atoms")
            ).nodes("//class[@lowered]"),
            Matchers.empty()
        );
    }

    @Test
    void stopsWhenSidecarIsAbsent(@Mktmp final Path temp) throws IOException {
        final XML xmir = StLoweredTest.lowered(StLoweredTest.program(), "94aabb03cd10");
        final Path atoms = Files.createDirectories(temp.resolve("atoms"));
        Assertions.assertThrows(
            RuntimeException.class,
            () -> StLoweredTest.transpiled(xmir, atoms),
            "a lowered formation without its sidecar must stop the build, but it didnt"
        );
    }

    private static String generated(final Path temp, final String digest) throws IOException {
        return StLoweredTest.transpiled(
            StLoweredTest.lowered(StLoweredTest.program(), digest),
            StLoweredTest.sidecars(temp, digest, "        return this.take(\"x\");")
        ).xpath("/object/class[@lowered='true']/java/text()").get(0);
    }

    private static XML transpiled(final XML xmir, final Path atoms) {
        return new Xsline(
            new TrDefault<Shift>()
                .with(new StClasspath("/org/eolang/parser/parse/set-locators.xsl"))
                .with(new StClasspath("/org/eolang/maven/transpile/set-original-names.xsl"))
                .with(new StClasspath("/org/eolang/maven/transpile/classes.xsl"))
                .with(new StClasspath("/org/eolang/maven/transpile/attrs.xsl"))
                .with(new StClasspath("/org/eolang/maven/transpile/purify.xsl"))
                .with(new StLowered("/org/eolang/maven/transpile/lowered.xsl", atoms))
                .with(new StClasspath("/org/eolang/maven/transpile/to-java.xsl"))
        ).pass(xmir);
    }

    private static XML program(final String... metas) throws IOException {
        return new EoSyntax(
            Stream.concat(
                Arrays.stream(metas),
                Stream.of(
                    "[args] > app",
                    "  [x] > bump",
                    "    x > @",
                    "  bump args > @",
                    ""
                )
            ).collect(Collectors.joining(System.lineSeparator()))
        ).parsed();
    }

    private static XML lowered(final XML parsed, final String digest) {
        final XMLDocument doc = new XMLDocument(parsed.toString());
        new Xembler(
            new Directives()
                .xpath("//o[@name='bump']")
                .attr("pure", "true")
                .attr("lowered", digest)
                .xpath("//o[@name='bump']/o[@name='@']")
                .remove()
                .xpath("//o[@name='bump']")
                .add("o")
                .attr("name", "λ")
                .attr("atom", "Φ.number")
        ).applyQuietly(doc.inner());
        return doc;
    }

    private static Path sidecars(
        final Path temp, final String digest, final String body
    ) throws IOException {
        final Path atoms = Files.createDirectories(temp.resolve("atoms"));
        Files.writeString(atoms.resolve(String.format("%s.java", digest)), body);
        return atoms;
    }
}
