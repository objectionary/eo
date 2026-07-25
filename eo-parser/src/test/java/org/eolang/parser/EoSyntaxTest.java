/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.TrDefault;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Set;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.text.StringEscapeUtils;
import org.apache.log4j.Level;
import org.cactoos.io.InputOf;
import org.cactoos.io.ResourceOf;
import org.cactoos.iterable.Mapped;
import org.cactoos.set.SetOf;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtStrictAfter;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;
import org.eolang.xax.XtoryMatcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.xml.sax.SAXParseException;

/**
 * Test case for {@link EoSyntax}.
 * @since 0.1
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.AvoidDuplicateLiterals"})
@Execution(ExecutionMode.SAME_THREAD)
@ExtendWith(LogProgress.class)
final class EoSyntaxTest {

    @Test
    void parsesSimpleCode() throws Exception {
        MatcherAssert.assertThat(
            "EoSyntax must generate valid XMIR from simple code",
            XhtmlMatchers.xhtml(
                new String(
                    new EoSyntax(
                        new ResourceOf("org/eolang/parser/fibonacci.eo")
                    ).parsed().toString().getBytes(),
                    StandardCharsets.UTF_8
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/object[@ms and @time and @version]",
                "/object/listing",
                "/object/metas/meta[head='meta2']",
                "/object/o[@name='fibo']"
            )
        );
    }

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void parsesSimpleCodeWithDebugMode() {
        final org.apache.log4j.Logger logger = org.apache.log4j.Logger.getLogger(EoSyntax.class);
        final Level previous = logger.getLevel();
        logger.setLevel(Level.DEBUG);
        try {
            Assertions.assertDoesNotThrow(
                new EoSyntax(
                    String.join(
                        System.lineSeparator(),
                        "[] > x-н, 1".concat(System.lineSeparator())
                    )
                )::parsed,
                "EO syntax should not fail in debug mode when program has errors"
            );
        } finally {
            logger.setLevel(previous);
        }
    }

    @Test
    void prohibitsMoreThanOneTailingEol() throws Exception {
        MatcherAssert.assertThat(
            "doesn't prohibit more than one tailing EOL",
            XhtmlMatchers.xhtml(
                new String(
                    new EoSyntax(
                        new InputOf(
                            String.join(
                                System.lineSeparator(),
                                "[] > foo",
                                "",
                                "",
                                "",
                                ""
                            )
                        )
                    ).parsed().toString().getBytes(StandardCharsets.UTF_8),
                    StandardCharsets.UTF_8
                )
            ),
            XhtmlMatchers.hasXPaths("/object/errors/error")
        );
    }

    @Test
    void printsProperListingEvenWhenSyntaxIsBroken() throws Exception {
        final String src = String.join(
            System.lineSeparator(),
            "[] > x-н, 1".concat(System.lineSeparator())
        );
        MatcherAssert.assertThat(
            "EO syntax is broken, but listing should be printed",
            XhtmlMatchers.xhtml(
                new String(
                    new EoSyntax(
                        new InputOf(src)
                    ).parsed().toString().getBytes(StandardCharsets.UTF_8),
                    StandardCharsets.UTF_8
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/object/errors/error",
                String.format("/object[listing='%s']", StringEscapeUtils.escapeXml11(src))
            )
        );
    }

    @Test
    void copiesListingCorrectly() throws Exception {
        final String src = new TextOf(
            new ResourceOf("org/eolang/parser/factorial.eo")
        ).asString();
        MatcherAssert.assertThat(
            "EoSyntax must copy listing to XMIR",
            new Xnav(
                new XMLDocument(
                    new String(
                        new EoSyntax(
                            new InputOf(src)
                        ).parsed().toString().getBytes(),
                        StandardCharsets.UTF_8
                    )
                ).inner()
            ).element("object").element("listing").text().get(),
            Matchers.containsString(StringEscapeUtils.escapeXml11(src))
        );
    }

    @ParameterizedTest
    @MethodSource("parsesSuccessfullyArgs")
    void parsesSuccessfully(final String code) {
        Assertions.assertDoesNotThrow(
            new EoSyntax(
                new InputOf(code)
            )::parsed,
            "EO syntax must be parsed successfully without exceptions (even with errors)"
        );
    }

    @Test
    void parsesArrow() throws IOException {
        MatcherAssert.assertThat(
            "EO object with name must be parsed successfully",
            new EoSyntax(
                new InputOf("1 > x")
            ).parsed(),
            XhtmlMatchers.hasXPaths(
                "/object/o[@base='Φ.number' and @name='x' and o[text()]]"
            )
        );
    }

    @Test
    void parsesNested() throws IOException {
        MatcherAssert.assertThat(
            "EO object with nested objects must be parsed successfully",
            new EoSyntax(
                new InputOf(
                    String.join(
                        System.lineSeparator(),
                        "[] > base",
                        "  memory 0 > x",
                        "  [self] > f",
                        "    v > @",
                        "      v".concat(System.lineSeparator())
                    )
                )
            ).parsed(),
            XhtmlMatchers.hasXPaths(
                "/object[count(o)=1]",
                "/object/o[@name='base' and count(o[not(@name='xi\uD83C\uDF35')])=2]",
                "/object/o[@name='base']/o[@name='x']",
                "/object/o[@name='base']/o[@name='f']"
            )
        );
    }

    @Test
    void parsesCanonicalEoProgram() throws Exception {
        MatcherAssert.assertThat(
            "We expect that all of the bytes contain a formation with data",
            new EoSyntax(
                new TextOf(
                    new ResourceOf("org/eolang/parser/canonical.eo")
                ).asString()
            ).parsed(),
            Matchers.not(XhtmlMatchers.hasXPath("//o[@base='Φ.bytes' and not(o)]"))
        );
    }

    @Test
    void homesBareReferenceIntoPackageWhenObjectExists() throws IOException {
        MatcherAssert.assertThat(
            "bare reference to a same-package object must be homed into the current package",
            new EoSyntax(
                new InputOf(
                    String.join(
                        System.lineSeparator(),
                        "+package foo",
                        "",
                        "[] > x",
                        "  bar 42 > @",
                        "  seq > y".concat(System.lineSeparator())
                    )
                ),
                new Canonical("foo.bar")
            ).parsed(),
            XhtmlMatchers.hasXPaths(
                "/object[not(errors)]",
                "//o[@base='Φ.foo.bar']",
                "//o[@base='Φ.seq']"
            )
        );
    }

    @Test
    void keepsBareReferenceAtRootWhenObjectAbsent() throws IOException {
        MatcherAssert.assertThat(
            "bare reference must default to the root Φ when the object is unknown",
            new EoSyntax(
                new InputOf(
                    String.join(
                        System.lineSeparator(),
                        "+package foo",
                        "",
                        "[] > x",
                        "  bar 42 > @".concat(System.lineSeparator())
                    )
                )
            ).parsed(),
            XhtmlMatchers.hasXPaths(
                "/object[not(errors)]",
                "//o[@base='Φ.bar']"
            )
        );
    }

    @Test
    void parsesMethodCalls() throws IOException {
        MatcherAssert.assertThat(
            "We expect EO object as method call is parsed successfully",
            new EoSyntax(
                new InputOf(
                    String.join(System.lineSeparator(), "add. > foo", "  0", "  true")
                )
            ).parsed(),
            XhtmlMatchers.hasXPaths(
                "/object/o[@base='.add']",
                "/object/o/o[@base='Φ.number']",
                "/object/o/o[@base='Φ.true']"
            )
        );
    }

    @Test
    void emitsGraphLocatorsForObjects() throws IOException {
        MatcherAssert.assertThat(
            "parsed objects must carry a graph locator anchored at Φ",
            new EoSyntax(
                new InputOf(
                    String.join(System.lineSeparator(), "[] > foo", "  42 > @")
                )
            ).parsed(),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='foo' and @loc='Φ.foo']",
                "/object/o[@name='foo']/o[@loc='Φ.foo.φ']"
            )
        );
    }

    @ParameterizedTest
    @ValueSource(
        strings = {
            "5 > five",
            "\"Hello\" > str"
        }
    )
    void storesAsBytes(final String code) throws IOException {
        MatcherAssert.assertThat(
            "We data is parsed successfully as bytes",
            new EoSyntax(new InputOf(code)).parsed(),
            XhtmlMatchers.hasXPaths(
                "/object[count(o)=1]",
                "/object/o[text()]"
            )
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/eo-typos/", glob = "**.yaml")
    @SuppressWarnings("PMD.UnitTestContainsTooManyAsserts")
    void checksTypoPacks(final String yaml) {
        final Xtory story = new XtSticky(
            new XtYaml(
                yaml,
                eo -> new EoSyntax(new InputOf(String.format("%s%n", eo))).parsed()
            )
        );
        Assumptions.assumeTrue(story.map().get("skip") == null);
        final Xnav after = new Xnav(story.after().inner());
        MatcherAssert.assertThat(
            "We expect the error with correct line number was found",
            after.path("/object/errors/error/@line").findAny().isPresent(),
            Matchers.equalTo(true)
        );
        MatcherAssert.assertThat(
            after.toString(),
            after.path("/object/errors/error/@line").map(line -> line.text().get())
                .collect(Collectors.toList()),
            Matchers.hasItem(story.map().get("line").toString())
        );
        final String msg = "message";
        if (story.map().containsKey(msg)) {
            MatcherAssert.assertThat(
                XhtmlMatchers.xhtml(story.after()).toString(),
                String.join(
                    System.lineSeparator(),
                    after.path("/object/errors/error").map(error -> error.text().get())
                        .collect(Collectors.toList())
                ).replaceAll("\\r", ""),
                Matchers.containsString(story.map().get(msg).toString())
            );
        }
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/eo-packs/", glob = "**.yaml")
    void checksEoPacks(final String yaml) {
        final Xtory story = new XtSticky(
            new XtStrictAfter(
                new XtYaml(
                    yaml,
                    eo -> new EoSyntax(
                        String.format("%s%n", eo), new TrDefault<>()
                    ).parsed(),
                    new TrFull()
                )
            )
        );
        Assumptions.assumeTrue(story.map().get("skip") == null);
        MatcherAssert.assertThat(
            "passed without exceptions",
            story,
            new XtoryMatcher()
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/eo-syntax/", glob = "**.yaml")
    void validatesEoSyntax(final String yaml) {
        MatcherAssert.assertThat(
            "passed without exceptions",
            new XtSticky(
                new XtYaml(
                    yaml,
                    eo -> new EoSyntax(String.format("%s%n", eo)).parsed()
                )
            ),
            new XtoryMatcher()
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/xsd-mistakes/", glob = "**.yaml")
    void checksXsdMistakes(final String yaml) throws Exception {
        final Xtory story = new XtSticky(
            new XtYaml(
                yaml,
                eo -> new EoSyntax(
                    new InputOf(String.format("%s%n", eo))
                ).parsed()
            )
        );
        Assumptions.assumeTrue(story.map().get("skip") == null);
        final Set<String> errors = new SetOf<>(
            new Mapped<>(
                SAXParseException::toString,
                story.after().validate(
                    new XMLDocument(
                        new TextOf(new ResourceOf("XMIR.xsd")).asString()
                    )
                )
            )
        );
        MatcherAssert.assertThat(
            Logger.format("correct number of errors found: %[list]s%n%s", errors, yaml),
            errors,
            Matchers.iterableWithSize(
                Integer.parseInt(story.map().get("errors").toString())
            )
        );
    }

    @Test
    void printsSyntaxWithComments() throws IOException {
        final Xnav xml = new Xnav(
            new EoSyntax(
                new InputOf(
                    String.join(
                        System.lineSeparator(),
                        "# Foo.",
                        "# Bar.",
                        "# Xyz.",
                        "",
                        "[] > foo"
                    )
                )
            ).parsed().inner()
        );
        final String comments = xml.element("object").element("comments").element("comment").text()
            .get();
        final String expected = String.format("Foo.%nBar.%nXyz.").replace(
            System.lineSeparator(), String.valueOf((char) 10)
        );
        MatcherAssert.assertThat(
            String.format(
                "EO parsed: %s, but comments: '%s' don't match with expected: '%s'",
                xml, comments, expected
            ),
            comments,
            Matchers.equalTo(expected)
        );
    }

    @ParameterizedTest
    @MethodSource("naughty")
    void parsesNaughtyString(final String input) throws IOException {
        MatcherAssert.assertThat(
            String.format("Failed to understand string: %s", input),
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[] > app",
                    String.format("  Q.io.stdout \"%s\" > @", input)
                )
            ).parsed(),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    @ParameterizedTest
    @CsvSource(
        {
            "#   Indented comment is here 守规矩!,\\n  Indented comment is here 守规矩!",
            "#     More indentation,\\n    More indentation",
            "#       This is how it works!,\\n      This is how it works!"
        }
    )
    void savesIndentationInComments(final String comment, final String parsed) throws IOException {
        MatcherAssert.assertThat(
            "Parsed comments in XMIR should respect indentation",
            new Xnav(
                new EoSyntax(
                    new InputOf(
                        String.join(
                            System.lineSeparator(),
                            "# Top comment.",
                            comment,
                            "",
                            "[] > foo"
                        )
                    )
                ).parsed().inner()
            ).element("object").element("comments").element("comment").text().get(),
            Matchers.equalTo(
                String.format("Top comment.%s", parsed)
                    .replace("\\n", String.valueOf((char) 10))
            )
        );
    }

    @Test
    void parsesEmptyComment() throws IOException {
        MatcherAssert.assertThat(
            "Parsed empty comments in XMIR should be empty as well",
            new Xnav(
                new EoSyntax(
                    new InputOf(
                        String.join(
                            System.lineSeparator(),
                            "#",
                            "",
                            "[] > foo"
                        )
                    )
                ).parsed().inner()
            ).element("object").element("comments").element("comment").text().get(),
            Matchers.emptyString()
        );
    }

    @Test
    void checksProhibitionCactusInObjectName() throws Exception {
        MatcherAssert.assertThat(
            "Cactus is prohibited in object name",
            XhtmlMatchers.xhtml(
                new String(
                    new EoSyntax(
                        new InputOf(
                            String.join(
                                System.lineSeparator(),
                                "[] > foo\uD83C\uDF35bar".concat(System.lineSeparator())
                            )
                        )
                    ).parsed().toString().getBytes(StandardCharsets.UTF_8),
                    StandardCharsets.UTF_8
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/object/errors/error[contains(text(),'cactus')]"
            )
        );
    }

    @Test
    void checksProhibitionCactusInAttributeName() throws Exception {
        MatcherAssert.assertThat(
            "Cactus is prohibited in attribute name",
            XhtmlMatchers.xhtml(
                new String(
                    new EoSyntax(
                        new InputOf(
                            String.join(
                                System.lineSeparator(),
                                "[] > app",
                                "  x > a\uD83C\uDF3565".concat(System.lineSeparator())
                            )
                        )
                    ).parsed().toString().getBytes(StandardCharsets.UTF_8),
                    StandardCharsets.UTF_8
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/object/errors/error[contains(text(),'cactus')]"
            )
        );
    }

    @Test
    void checksProhibitionCactusInAttributeValue() throws Exception {
        MatcherAssert.assertThat(
            "Cactus is prohibited in attribute value",
            XhtmlMatchers.xhtml(
                new String(
                    new EoSyntax(
                        new InputOf(
                            String.join(
                                System.lineSeparator(),
                                "[] > x",
                                "  \uD83C\uDF35 > y".concat(System.lineSeparator())
                            )
                        )
                    ).parsed().toString().getBytes(StandardCharsets.UTF_8),
                    StandardCharsets.UTF_8
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/object/errors/error[contains(text(),'cactus')]"
            )
        );
    }

    @Test
    void wrapsSourceInObjectAndListing() throws Exception {
        MatcherAssert.assertThat(
            "the parser must produce an <object> with a <listing> carrying the source",
            EoSyntaxTest.raw("[] > foo").toString(),
            XhtmlMatchers.hasXPaths(
                "/object",
                "/object/listing",
                "/object/o[@name='foo']"
            )
        );
    }

    @Test
    void parsesMetaUnderObjectRoot() throws Exception {
        MatcherAssert.assertThat(
            "metas emitted by the walker must appear under /object/metas in the final XMIR",
            EoSyntaxTest.raw("+alias org.example.foo").toString(),
            XhtmlMatchers.hasXPaths(
                "/object/metas/meta/head[text()='alias']",
                "/object/metas/meta/part[text()='org.example.foo']"
            )
        );
    }

    @Test
    void parsesFormationWithVoidParameters() throws Exception {
        MatcherAssert.assertThat(
            "void parameters of a formation must appear as <o base='∅'/> children",
            EoSyntaxTest.raw("[a b] > main").toString(),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='main']/o[@name='a' and @base='∅']",
                "/object/o[@name='main']/o[@name='b' and @base='∅']"
            )
        );
    }

    @Test
    void surfacesParseErrorsInline() throws Exception {
        MatcherAssert.assertThat(
            "a tab in leading whitespace must show up as an /object/errors/error entry",
            EoSyntaxTest.raw("\tfoo").toString(),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'tab character in leading whitespace')]"
            )
        );
    }

    @Test
    void rejectsOutOfRangeOctalEscape() throws Exception {
        MatcherAssert.assertThat(
            "an out-of-range \\NNN octal escape (value > 0o377) must show up as an /object/errors/error entry, not silently emit a multi-byte value",
            EoSyntaxTest.raw(
                String.join(String.valueOf((char) 10), "[] > foo", "  \"\\477\" > @")
            ).toString(),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'octal')]"
            )
        );
    }

    @Test
    void emitsProgramMetadataAttributes() throws Exception {
        MatcherAssert.assertThat(
            "the <object> root must carry the standard program metadata attributes",
            EoSyntaxTest.raw("+foo").toString(),
            XhtmlMatchers.hasXPath("/object[@version and @revision and @dob and @time]")
        );
    }

    /**
     * Prepare naughty strings.
     * @return Stream of strings
     * @throws IOException if I/O fails
     */
    private static Stream<Arguments> naughty() throws IOException {
        return Files.readAllLines(Paths.get("target/blns.txt")).stream().filter(s -> !s.isEmpty())
            .map(StringEscapeUtils::escapeJava)
            .map(Arguments::of);
    }

    /**
     * Parse a single-line EO source with no post-XSL transform — the
     * resulting XMIR shows the raw parser output, useful for asserting
     * directly on the parser's emission shape.
     * @param line One EO source line
     * @return Raw XMIR
     * @throws Exception If parsing fails
     */
    private static XML raw(final String line) throws Exception {
        return new EoSyntax(
            new InputOf(line.concat(String.valueOf((char) 10))),
            UnaryOperator.identity()
        ).parsed();
    }

    /**
     * Inputs for {@link EoSyntaxTest#parsesSuccessfully}.
     * @return Test cases
     */
    private static Stream<String> parsesSuccessfullyArgs() {
        final String eol = String.valueOf((char) 10);
        final String crlf = String.valueOf((char) 13).concat(eol);
        return Stream.of(
            String.join(eol, "1 > x", "2 > y"),
            String.join(crlf, "1 > x", "2 > y"),
            String.join(crlf.concat(crlf), "1 > x", "2 > y"),
            String.join(eol, "1 > x", "2 > y").concat(eol),
            String.join(eol.concat(eol), "1 > x", "2 > y"),
            "[] > x",
            String.join(eol, "[] > x", "  x ^ > @")
        );
    }
}
