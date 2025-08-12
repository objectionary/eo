/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
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
import java.util.stream.Stream;
import org.apache.commons.text.StringEscapeUtils;
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
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.xml.sax.SAXParseException;

/**
 * Test case for {@link EoSyntax}.
 *
 * @since 0.1
 * @todo #4052:35min Replace XML.xpath() with Xnav.path().
 *  Currently, in {@link EoSyntaxTest#checksTypoPacks} its blocked by attribute parsing issue in
 *  Xnav. Please check <a href="https://github.com/volodya-lombrozo/xnav/issues/119">this</a> issue
 *  for more details. Once it will be resolved, we should proceed with the replacement.
 */
@SuppressWarnings("PMD.TooManyMethods")
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
    @Disabled
    void prohibitsMoreThanOneTailingEol() throws Exception {
        MatcherAssert.assertThat(
            "doesn't prohibit more than one tailing EOL",
            XhtmlMatchers.xhtml(
                new String(
                    new EoSyntax(
                        new InputOf("# No comments.\n[] > foo\n\n\n\n")
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
            "\n",
            "# No comments.",
            "[] > x-н, 1\n"
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
                "/object/errors[count(error)=2]",
                String.format("/object[listing='%s']", StringEscapeUtils.escapeXml11(src))
            )
        );
    }

    @Test
    void copiesListingCorrectly() throws Exception {
        final String src = new TextOf(
            new ResourceOf("org/eolang/parser/factorial.eo")
        ).asString();
        final Xnav xml = new Xnav(
            new XMLDocument(
                new String(
                    new EoSyntax(
                        new InputOf(src)
                    ).parsed().toString().getBytes(),
                    StandardCharsets.UTF_8
                )
            ).inner()
        );
        MatcherAssert.assertThat(
            "EoSyntax must copy listing to XMIR",
            xml.element("object").element("listing").text().get(),
            Matchers.containsString(StringEscapeUtils.escapeXml11(src))
        );
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "1 > x\n2 > y",
        "1 > x\r\n2 > y",
        "1 > x\r\n\r\n2 > y",
        "1 > x\n2 > y\n",
        "1 > x\n\n2 > y",
        "# No comments.\n[] > x",
        "# No comments.\n[] > x\n  x ^ > @"
    })
    void parsesSuccessfully(final String code) {
        final EoSyntax syntax = new EoSyntax(
            new InputOf(code)
        );
        Assertions.assertDoesNotThrow(
            syntax::parsed,
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
                "/object/o[@base='Φ.org.eolang.number' and @name='x' and o[text()]]"
            )
        );
    }

    @Test
    void parsesNested() throws IOException {
        final String src = String.join(
            "\n",
            "# No comments.",
            "[] > base",
            "  memory 0 > x",
            "  # No comments.",
            "  [self] > f",
            "    v > @",
            "      v\n"
        );
        MatcherAssert.assertThat(
            "EO object with nested objects must be parsed successfully",
            new EoSyntax(
                new InputOf(src)
            ).parsed(),
            XhtmlMatchers.hasXPaths(
                "/object[count(o)=1]",
                "/object/o[count(o)=2]"
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
            Matchers.not(XhtmlMatchers.hasXPath("//o[@base='Φ.org.eolang.bytes' and not(o)]"))
        );
    }

    @Test
    void parsesMethodCalls() throws IOException {
        MatcherAssert.assertThat(
            "We expect EO object as method call is parsed successfully",
            new EoSyntax(
                new InputOf("add. > foo\n  0\n  true")
            ).parsed(),
            XhtmlMatchers.hasXPaths(
                "/object/o[@base='.add']",
                "/object/o/o[@base='Φ.org.eolang.number']",
                "/object/o/o[@base='Φ.org.eolang.true']"
            )
        );
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "5 > five",
        "\"Hello\" > str"
    })
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
    void checksTypoPacks(final String yaml) {
        final Xtory story = new XtSticky(
            new XtYaml(
                yaml,
                eo -> new EoSyntax(new InputOf(String.format("%s\n", eo))).parsed()
            )
        );
        Assumptions.assumeTrue(story.map().get("skip") == null);
        MatcherAssert.assertThat(
            "We expect the error with correct line number was found",
            XhtmlMatchers.xhtml(story.after().toString()),
            XhtmlMatchers.hasXPaths("/object/errors/error/@line")
        );
        MatcherAssert.assertThat(
            XhtmlMatchers.xhtml(story.after()).toString(),
            story.after().xpath("/object/errors/error/@line"),
            Matchers.hasItem(story.map().get("line").toString())
        );
        final String msg = "message";
        if (story.map().containsKey(msg)) {
            MatcherAssert.assertThat(
                XhtmlMatchers.xhtml(story.after()).toString(),
                String.join(
                    "\n",
                    story.after().xpath("/object/errors/error/text()")
                ).replaceAll("\r", ""),
                Matchers.containsString(story.map().get(msg).toString())
            );
        }
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/eo-packs/", glob = "**.yaml")
    void checksEoPacks(final String yaml) {
        MatcherAssert.assertThat(
            "passed without exceptions",
            new XtSticky(
                new XtStrictAfter(
                    new XtYaml(
                        yaml,
                        eo -> new EoSyntax(
                            String.format("%s\n", eo), new TrDefault<>()
                        ).parsed(),
                        new TrFull()
                    )
                )
            ),
            new XtoryMatcher()
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/eo-syntax/", glob = "**.yaml")
    void validatesEoSyntax(final String yaml) {
        MatcherAssert.assertThat(
            "passed without exceptions",
            new XtSticky(
                new XtSticky(
                    new XtYaml(
                        yaml,
                        eo -> new EoSyntax(String.format("%s\n", eo)).parsed()
                    )
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
                    new InputOf(String.format("%s\n", eo))
                ).parsed()
            )
        );
        Assumptions.assumeTrue(story.map().get("skip") == null);
        final XML xml = story.after();
        final Set<String> errors = new SetOf<>(
            new Mapped<>(
                SAXParseException::toString,
                xml.validate(
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
                        "\n",
                        "# Foo.",
                        "# Bar.",
                        "# Xyz.",
                        "[] > foo"
                    )
                )
            ).parsed().inner()
        );
        final String comments = xml.element("object").element("comments").element("comment").text()
            .get();
        final String expected = "Foo.\\nBar.\\nXyz.";
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
                    "\n",
                    "# App.",
                    "[] > app",
                    String.format("  QQ.io.stdout \"%s\" > @", input)
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
                            "\n",
                            "# Top comment.",
                            comment,
                            "[] > foo"
                        )
                    )
                ).parsed().inner()
            ).element("object").element("comments").element("comment").text().get(),
            Matchers.equalTo(String.format("Top comment.%s", parsed))
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
                            "\n",
                            "#",
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
        final String src = String.join(
            "\n",
            "[] > foo\uD83C\uDF35bar\n"
        );
        MatcherAssert.assertThat(
            "Cactus is prohibited in object name",
            XhtmlMatchers.xhtml(
                new String(
                    new EoSyntax(
                        new InputOf(src)
                    ).parsed().toString().getBytes(StandardCharsets.UTF_8),
                    StandardCharsets.UTF_8
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/object/errors[count(error)=2]"
            )
        );
    }

    @Test
    void checksProhibitionCactusInAttributeName() throws Exception {
        final String src = String.join(
            "\n",
            "[] > app",
            "  x > a\uD83C\uDF3565\n"
        );
        MatcherAssert.assertThat(
            "Cactus is prohibited in attribute name",
            XhtmlMatchers.xhtml(
                new String(
                    new EoSyntax(
                        new InputOf(src)
                    ).parsed().toString().getBytes(StandardCharsets.UTF_8),
                    StandardCharsets.UTF_8
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/object/errors[count(error)=2]"
            )
        );
    }

    @Test
    void checksProhibitionCactusInAttributeValue() throws Exception {
        final String src = String.join(
            "\n",
            "[] > x",
            "  \uD83C\uDF35 > y\n"
        );
        MatcherAssert.assertThat(
            "Cactus is prohibited in attribute value",
            XhtmlMatchers.xhtml(
                new String(
                    new EoSyntax(
                        new InputOf(src)
                    ).parsed().toString().getBytes(StandardCharsets.UTF_8),
                    StandardCharsets.UTF_8
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/object/errors[count(error)=2]"
            )
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
}
