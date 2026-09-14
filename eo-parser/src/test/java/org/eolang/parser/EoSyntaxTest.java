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
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Train;
import fixtures.LargeProgram;
import java.io.IOException;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Set;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;
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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
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
 */
@ExtendWith(LogProgress.class)
final class EoSyntaxTest {

    @Test
    void runsWithoutSingleThreadRestriction() {
        MatcherAssert.assertThat(
            "class still carries an execution mode restriction",
            EoSyntaxTest.class.isAnnotationPresent(Execution.class),
            Matchers.is(false)
        );
    }

    @Test
    void carriesNoSharedStaticState() {
        MatcherAssert.assertThat(
            "EoSyntax declares a shared static field instead of building state per instance",
            Arrays.stream(EoSyntax.class.getDeclaredFields())
                .filter(field -> Modifier.isStatic(field.getModifiers()))
                .count(),
            Matchers.equalTo(0L)
        );
    }

    @Test
    void parsesSimpleCode() throws Exception {
        MatcherAssert.assertThat(
            "EoSyntax must generate valid XMIR from simple code",
            XhtmlMatchers.xhtml(
                new EoSyntax(new ResourceOf("org/eolang/parser/fibonacci.eo"))
                    .parsed().toString()
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
    void measuresRealParsingTime() throws Exception {
        MatcherAssert.assertThat(
            "ms attribute is not a measured elapsed time",
            Long.parseLong(
                new EoSyntax(
                    new LargeProgram(30), UnaryOperator.<XML>identity()
                ).parsed().xpath("/object/@ms").get(0)
            ),
            Matchers.greaterThan(0L)
        );
    }

    @Test
    void measuresSubMillisecondParsingTime() throws Exception {
        final EoSyntax syntax = new EoSyntax(String.format("# Ünïcödé.%n[] > tiny%n"));
        syntax.parsed();
        MatcherAssert.assertThat(
            "ms attribute of a sub-millisecond parse is not rounded up to one",
            Long.parseLong(syntax.parsed().xpath("/object/@ms").get(0)),
            Matchers.greaterThan(0L)
        );
    }

    @Test
    void reportsMsWithinSaneBound() throws Exception {
        MatcherAssert.assertThat(
            "ms attribute is not within a sane bound for a small program",
            Long.parseLong(
                new EoSyntax(
                    new ResourceOf("org/eolang/parser/fibonacci.eo")
                ).parsed().xpath("/object/@ms").get(0)
            ),
            Matchers.lessThan(60_000L)
        );
    }

    @Test
    void measuresParsingTimeOnEveryCall() throws Exception {
        final EoSyntax syntax = new EoSyntax(
            new LargeProgram(30), UnaryOperator.<XML>identity()
        );
        syntax.parsed();
        MatcherAssert.assertThat(
            "second parse of the same syntax does not measure its own elapsed time",
            Long.parseLong(syntax.parsed().xpath("/object/@ms").get(0)),
            Matchers.greaterThan(0L)
        );
    }

    @Test
    void rejectsANullTransform() {
        Assertions.assertThrows(
            NullPointerException.class,
            () -> new EoSyntax(new InputOf(""), (UnaryOperator<XML>) null).parsed(),
            "EoSyntax must reject a null transform, but it didn't"
        );
    }

    @Test
    void rejectsANullTransformAtConstructionTime() {
        Assertions.assertThrows(
            NullPointerException.class,
            () -> new EoSyntax(new InputOf(""), (UnaryOperator<XML>) null),
            "EoSyntax must reject a null transform at construction, before parsed() is ever called"
        );
    }

    @Test
    void acceptsANonNullTransformAtConstructionTime() {
        Assertions.assertDoesNotThrow(
            () -> new EoSyntax(new InputOf(""), UnaryOperator.identity()),
            "EoSyntax must accept a non-null transform at construction, but it didn't"
        );
    }

    @Test
    void rejectsANullTrain() {
        Assertions.assertThrows(
            NullPointerException.class,
            () -> new EoSyntax(new InputOf(""), (Train<Shift>) null).parsed(),
            "EoSyntax must reject a null train, but it didn't"
        );
    }

    @Test
    void acceptsANonNullTrain() throws Exception {
        MatcherAssert.assertThat(
            "EoSyntax must parse code with a non-null train, but it didn't",
            XhtmlMatchers.xhtml(
                new EoSyntax("[] > foo", new TrDefault<Shift>()).parsed().toString()
            ),
            XhtmlMatchers.hasXPath("/object/o[@name='foo']")
        );
    }

    @Test
    void prohibitsMoreThanOneTailingEol() throws Exception {
        MatcherAssert.assertThat(
            "doesn't prohibit more than one tailing EOL",
            XhtmlMatchers.xhtml(
                new EoSyntax(
                    new InputOf(
                        String.join(
                            System.lineSeparator(), "[] > foo", "", "", "", ""
                        )
                    )
                ).parsed().toString()
            ),
            XhtmlMatchers.hasXPaths("/object/errors/error")
        );
    }

    @Test
    void printsProperListingEvenWhenSyntaxIsBroken() throws Exception {
        final String src = "[] > x-н, 1".concat(String.valueOf((char) 10));
        MatcherAssert.assertThat(
            "EO syntax is broken, but listing should be printed",
            new Xnav(
                new EoSyntax(new InputOf(src)).parsed().inner()
            ).element("object").element("listing").text().get(),
            Matchers.equalTo(src)
        );
    }

    @Test
    void printsErrorsWhenSyntaxIsBroken() throws Exception {
        MatcherAssert.assertThat(
            "EO syntax is broken, thus errors should be printed",
            XhtmlMatchers.xhtml(
                new EoSyntax(
                    new InputOf("[] > x-н, 1".concat(System.lineSeparator()))
                ).parsed().toString()
            ),
            XhtmlMatchers.hasXPaths("/object/errors/error")
        );
    }

    @Test
    void reportsErrorOnLineWithCharacterForbiddenInXml() throws Exception {
        MatcherAssert.assertThat(
            "a broken line quoting a forbidden character must still produce an <error>",
            new EoSyntax(
                new InputOf(String.format("[] > x-%cn, 1%n", 0x07))
            ).parsed(),
            XhtmlMatchers.hasXPaths("/object/errors/error")
        );
    }

    @Test
    void keepsCommentWithCharacterForbiddenInXml() throws Exception {
        MatcherAssert.assertThat(
            "a comment carrying a forbidden character must still reach <comments>",
            new EoSyntax(
                new InputOf(String.format("# note %c here%n%n[] > x%n", 0x07))
            ).parsed(),
            XhtmlMatchers.hasXPaths("/object/comments/comment[.='note  here']")
        );
    }

    @Test
    void rejectsProgramOfMetasAlone() throws Exception {
        MatcherAssert.assertThat(
            "a file of metas alone declares no object and must be refused",
            new EoSyntax(new InputOf(String.format("+package foo%n"))).parsed(),
            XhtmlMatchers.hasXPaths(
                "/object/errors/error[@check='validate-object-presence' and @severity='critical']"
            )
        );
    }

    @Test
    void rejectsProgramOfCommentsAlone() throws Exception {
        MatcherAssert.assertThat(
            "a file of a top comment block alone declares no object and must be refused",
            new EoSyntax(new InputOf(String.format("# just a note%n"))).parsed(),
            XhtmlMatchers.hasXPaths(
                "/object/errors/error[@check='validate-object-presence' and @severity='critical']"
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
                    new EoSyntax(new InputOf(src)).parsed().toString()
                ).inner()
            ).element("object").element("listing").text().get(),
            Matchers.equalTo(src)
        );
    }

    @Test
    void keepsListingVerbatimWithXmlSpecialCharacters() throws Exception {
        final String src = String.join(
            String.valueOf((char) 10),
            "# Sample.",
            "[] > app",
            "  \"a < b & c > d\" > x",
            ""
        );
        MatcherAssert.assertThat(
            "listing must hold the source verbatim, not XML-escaped",
            new Xnav(
                new EoSyntax(new InputOf(src)).parsed().inner()
            ).element("object").element("listing").text().get(),
            Matchers.equalTo(src)
        );
    }

    @Test
    void keepsListingVerbatimWithCrlf() throws Exception {
        final String src = String.join(
            String.valueOf((char) 13).concat(String.valueOf((char) 10)),
            "# Sample.",
            "[] > app",
            "  \"a < b & c > d\" > x",
            ""
        );
        MatcherAssert.assertThat(
            "listing must hold CRLF verbatim, regardless of platform",
            new Xnav(
                new EoSyntax(new InputOf(src)).parsed().inner()
            ).element("object").element("listing").text().get(),
            Matchers.equalTo(src)
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
                "/object/o[@name='base' and count(o[not(starts-with(@name, 'a🌵'))])=2]",
                "/object/o[@name='base']/o[@name='x']",
                "/object/o[@name='base']/o[@name='f']"
            )
        );
    }

    @Test
    void parsesCanonicalEoProgram() throws Exception {
        MatcherAssert.assertThat(
            "a formation came out with empty bytes",
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
            "data was not stored as bytes",
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
        final Xtory story = EoSyntaxTest.typo(yaml);
        MatcherAssert.assertThat(
            String.format(
                "no error was reported on line %s of %s",
                story.map().get("line"), yaml
            ),
            new Xnav(story.after().inner())
                .path("/object/errors/error/@line").map(line -> line.text().get())
                .collect(Collectors.toList()),
            Matchers.hasItem(story.map().get("line").toString())
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/eo-typos/", glob = "**.yaml")
    void checksTypoMessages(final String yaml) {
        final Xtory story = EoSyntaxTest.typo(yaml);
        final String msg = "message";
        Assumptions.assumeTrue(story.map().containsKey(msg));
        MatcherAssert.assertThat(
            XhtmlMatchers.xhtml(story.after()).toString(),
            String.join(
                System.lineSeparator(),
                new Xnav(story.after().inner()).path("/object/errors/error")
                    .map(error -> error.text().get())
                    .collect(Collectors.toList())
            ).replaceAll("\\r", ""),
            Matchers.containsString(story.map().get(msg).toString())
        );
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
            String.format("pack XPaths do not match the parsed XMIR in %s", yaml),
            story,
            new XtoryMatcher()
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/parser/eo-syntax/", glob = "**.yaml")
    void validatesEoSyntax(final String yaml) {
        MatcherAssert.assertThat(
            String.format("pack XPaths do not match the parsed XMIR in %s", yaml),
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
                new EoSyntax(
                    new InputOf(
                        "[] > foo🌵bar".concat(System.lineSeparator())
                    )
                ).parsed().toString()
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
                new EoSyntax(
                    new InputOf(
                        String.join(
                            System.lineSeparator(),
                            "[] > app",
                            "  x > a🌵65".concat(System.lineSeparator())
                        )
                    )
                ).parsed().toString()
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
                new EoSyntax(
                    new InputOf(
                        String.join(
                            System.lineSeparator(),
                            "[] > x",
                            "  🌵 > y".concat(System.lineSeparator())
                        )
                    )
                ).parsed().toString()
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
    void parsesEmptySourceIntoSchemaValidXmir() {
        Assertions.assertDoesNotThrow(
            () -> new StrictXmir(new EoSyntax("").parsed()).toString(),
            "XMIR of an empty source must match XMIR.xsd, which has no room for an empty <listing>"
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
    void rejectsLoneSurrogateUnicodeEscape() throws Exception {
        MatcherAssert.assertThat(
            "a \\u escape decoding to a lone UTF-16 surrogate (D800-DFFF) must show up as an /object/errors/error entry, not silently emit '?'",
            EoSyntaxTest.raw(
                String.join(String.valueOf((char) 10), "[] > foo", "  \"\\uD800\" > @")
            ).toString(),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'unicode')]"
            )
        );
    }

    @Test
    void rejectsSignedUnicodeEscape() throws Exception {
        MatcherAssert.assertThat(
            "a \\u escape with a leading sign character must show up as an /object/errors/error entry, not silently consume the sign as a hex digit",
            EoSyntaxTest.raw(
                String.join(String.valueOf((char) 10), "[] > foo", "  \"\\u+041\" > @")
            ).toString(),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'unicode')]"
            )
        );
    }

    @Test
    void rejectsTruncatedUnicodeEscape() throws Exception {
        MatcherAssert.assertThat(
            "a \\u escape with fewer than four hex digits must show up as an /object/errors/error entry, not pass through as literal text",
            EoSyntaxTest.raw(
                String.join(String.valueOf((char) 10), "[] > foo", "  \"\\u41\" > @")
            ).toString(),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'unicode')]"
            )
        );
    }

    @Test
    void rejectsUnrecognisedEscapeSequence() throws Exception {
        MatcherAssert.assertThat(
            "an unrecognised escape sequence must name the offending characters, not blame unicode or octal escapes",
            EoSyntaxTest.raw(
                String.join(String.valueOf((char) 10), "[] > foo", "  \"\\q\" > @")
            ).toString(),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),\"unrecognised escape sequence\")]"
            )
        );
    }

    @Test
    void namesLoneSurrogateInErrorMessage() throws Exception {
        MatcherAssert.assertThat(
            "a lone surrogate escape must name the offending codepoint, not blame unicode or octal escapes generically",
            EoSyntaxTest.raw(
                String.join(String.valueOf((char) 10), "[] > foo", "  \"\\uD800\" > @")
            ).toString(),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'lone surrogate')]"
            )
        );
    }

    @Test
    void acceptsValidSurrogatePairEscape() throws Exception {
        MatcherAssert.assertThat(
            "a high surrogate immediately followed by a low surrogate is a valid pair and must not be rejected",
            EoSyntaxTest.raw(
                String.join(String.valueOf((char) 10), "[] > foo", "  \"\\uD83C\\uDF08\" > @")
            ).toString(),
            Matchers.not(XhtmlMatchers.hasXPath("/object/errors/error"))
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

    private static Stream<Arguments> naughty() throws Exception {
        return new TextOf(new ResourceOf("org/eolang/parser/blns.txt")).asString()
            .lines().filter(s -> !s.isEmpty())
            .map(StringEscapeUtils::escapeJava)
            .map(Arguments::of);
    }

    private static Xtory typo(final String yaml) {
        final Xtory story = new XtSticky(
            new XtYaml(
                yaml,
                eo -> new EoSyntax(new InputOf(String.format("%s%n", eo))).parsed()
            )
        );
        Assumptions.assumeTrue(story.map().get("skip") == null);
        return story;
    }

    private static XML raw(final String source) throws Exception {
        return new EoSyntax(
            new InputOf(source.concat(String.valueOf((char) 10))),
            UnaryOperator.identity()
        ).parsed();
    }

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
