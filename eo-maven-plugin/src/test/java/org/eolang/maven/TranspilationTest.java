/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * Test case for {@link Transpilation}.
 *
 * @since 0.74
 */
@ExtendWith(MktmpResolver.class)
final class TranspilationTest {

    @Test
    void tellsTrackedStepsApartInTheCacheKey() {
        MatcherAssert.assertThat(
            "a build that writes the XMIRs of the train must not take the result of one that didnt",
            this.transpilation(new Tracking(true, false)).version(),
            Matchers.not(
                Matchers.equalTo(this.transpilation(new Tracking(false, false)).version())
            )
        );
    }

    @Test
    void tellsInferenceTablesApartInTheCacheKey(@Mktmp final Path temp) throws IOException {
        final Path tables = Files.createDirectories(temp.resolve("tables"));
        Files.writeString(tables.resolve("provides.xml"), "<provides><type id='Q.f'/></provides>");
        MatcherAssert.assertThat(
            "a build that read the rows of this object must not take one that had none",
            this.transpilation(tables).version(Collections.singletonList("Q.f")),
            Matchers.not(
                Matchers.equalTo(
                    this.transpilation(temp.resolve("absent"))
                        .version(Collections.singletonList("Q.f"))
                )
            )
        );
    }

    @Test
    void tellsDeadlinesApartInTheCacheKey() {
        MatcherAssert.assertThat(
            "a build whose tests run under one deadline took the result of one whose tests run under another",
            this.transpilation(3L, "1G").version(),
            Matchers.not(Matchers.equalTo(this.transpilation(11L, "1G").version()))
        );
    }

    @Test
    void tellsMemoryBudgetsApartInTheCacheKey() {
        MatcherAssert.assertThat(
            "a build whose tests run under one memory budget took the result of one whose tests run under another",
            this.transpilation(1L, "257M").version(),
            Matchers.not(Matchers.equalTo(this.transpilation(1L, "3G").version()))
        );
    }

    @Test
    void passesGivenDeadlineToGeneratedTimeout(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "The given deadline didnt reach the timeout of a generated test",
            TranspilationTest.tested(this.transpilation(13L, "1G", temp)),
            Matchers.containsString("@Timeout(value = 13, unit = TimeUnit.SECONDS)")
        );
    }

    @Test
    void passesGivenMemoryToGeneratedBudget(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "The given memory budget didnt reach the budget of a generated test",
            TranspilationTest.tested(this.transpilation(1L, "389M", temp)),
            Matchers.containsString("@Budget(\"389M\")")
        );
    }

    @Test
    void foldsInImportedXslLibrariesIntoVersion() {
        MatcherAssert.assertThat(
            "the cache-key version must differ from a fingerprint of the top-level XSLS alone, proving the xsl:import-ed libraries are actually folded in",
            this.transpilation(new Tracking(false, false)).version(),
            Matchers.not(
                Matchers.startsWith(new Fingerprint(Transpilation.XSLS).get())
            )
        );
    }

    @Test
    void buildsSourceFunctionForParentlessMeasuresPath() {
        Assertions.assertDoesNotThrow(
            () -> new Transpilation(
                new Tracking(false, false),
                false,
                "PhDefault",
                1L,
                "1G",
                Paths.get("xsl-measures.csv"),
                Paths.get("target"),
                Paths.get("target/eo/6-inference")
            ).forSource("foo"),
            "forSource() must not throw when eo.xslMeasuresFile is a bare relative path with no parent directory"
        );
    }

    @Test
    void avoidsLiteralShortcutForCustomPhiClass() throws IOException {
        MatcherAssert.assertThat(
            "custom phiDefaultClass output must not use PhLiteral",
            this.transpiled(
                TranspilationTest.instantiating(), "org.example.PhInspected"
            ),
            Matchers.not(Matchers.containsString("new PhLiteral"))
        );
    }

    @Test
    void emitsLiteralShortcutForExactLiteralShapes() throws IOException {
        final String java = this.transpiled(TranspilationTest.literals());
        MatcherAssert.assertThat(
            "known literal objects must be decorated exactly once",
            Stream.of(
                "new PhLiteral",
                "new PhLiteral(r, new byte[] {(byte) 0x40",
                "new PhLiteral(r, new byte[] {(byte) 0xE9",
                "new PhLiteral(r, new byte[] {(byte) 0x01"
            ).map(
                pattern -> java.lines()
                    .filter(line -> line.contains(pattern)).count()
            ).toList(),
            Matchers.equalTo(Arrays.asList(5L, 1L, 1L, 1L))
        );
    }

    @Test
    void avoidsShortcutForComputedLiteralForma() throws IOException {
        MatcherAssert.assertThat(
            "a number whose phi is an expression must remain on the legacy path",
            this.transpiled(TranspilationTest.computedLiteral()),
            Matchers.not(Matchers.containsString("new PhLiteral"))
        );
    }

    @Test
    void avoidsShortcutForExtraLiteralArgument() throws IOException {
        MatcherAssert.assertThat(
            "an extra application argument must keep the legacy outer object",
            TranspilationTest.attribute(
                this.transpiled(TranspilationTest.literalShape("42", "43")), "n"
            ),
            Matchers.not(Matchers.containsString(TranspilationTest.literal()))
        );
    }

    @Test
    void avoidsShortcutForComputedBytesArgument() throws IOException {
        MatcherAssert.assertThat(
            "a computed bytes argument must keep the legacy outer object",
            TranspilationTest.attribute(
                this.transpiled(TranspilationTest.literalShape("42.plus 1")), "n"
            ),
            Matchers.not(Matchers.containsString(TranspilationTest.literal()))
        );
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("postDataShapes")
    void matchesOnlySafePostDataBindings(final Shape shape) throws Exception {
        final XML xmir = TranspilationTest.postDataXml(shape);
        final String java = TranspilationTest.attribute(
            TranspilationTest.javaFromPostData(xmir), "n"
        );
        MatcherAssert.assertThat(
            shape.description(),
            Arrays.asList(
                xmir.nodes(shape.bindingSelector()).size() == 1,
                xmir.nodes(shape.levelSelector()).size() == 1,
                shape.valueCount().equals(
                    xmir.xpath("count(//o[@name='n']/value)").get(0)
                ),
                shape.shortcutMatches(java),
                shape.zeroBindingMatches(java)
            ),
            Matchers.everyItem(Matchers.is(true))
        );
    }

    private static String instantiating() {
        return String.join(
            System.lineSeparator(),
            "+architect yegor256@gmail.com",
            "+package foo.x",
            "",
            "[] > main",
            "  [] > inner",
            "    42 > @",
            "  42.plus > @",
            "    []"
        );
    }

    private static String literal() {
        return "new PhLiteral(r, ";
    }

    private static String literals() {
        return String.join(
            System.lineSeparator(),
            "+architect yegor256@gmail.com",
            "+package foo.x",
            "",
            "[] > main",
            "  42 > number",
            "  \"雪だるま\" > string",
            "  01-AF > bytes"
        );
    }

    private static String computedLiteral() {
        return String.join(
            System.lineSeparator(),
            "+architect yegor256@gmail.com",
            "+package foo.x",
            "",
            "[x] > main",
            "  number x.as-bytes > @"
        );
    }

    private static String literalShape(final String... children) {
        return String.join(
            System.lineSeparator(),
            "+architect yegor256@gmail.com",
            "+package foo.x",
            "",
            "[] > main",
            "  number > n",
            Arrays.stream(children).map("    "::concat)
                .collect(Collectors.joining(System.lineSeparator()))
        );
    }

    private String transpiled(final String source) throws IOException {
        return this.transpiled(source, "PhDefault");
    }

    private String transpiled(final String source, final String superclass)
        throws IOException {
        return String.join(
            "",
            this.transpilation(new Tracking(false, false), superclass)
                .forSource("foo.x.main")
                .apply(new EoSyntax(source).parsed())
                .xpath("//java/text()")
        );
    }

    private Transpilation transpilation(final Tracking tracking) {
        return this.transpilation(tracking, "PhDefault");
    }

    private Transpilation transpilation(
        final Tracking tracking, final String superclass
    ) {
        return new Transpilation(
            tracking,
            false,
            superclass,
            1L,
            "1G",
            Paths.get("xsl-measures.csv"),
            Paths.get("target"),
            Paths.get("target/eo/6-inference")
        );
    }

    private Transpilation transpilation(final long deadline, final String memory) {
        return new Transpilation(
            new Tracking(false, false),
            false,
            "PhDefault",
            deadline,
            memory,
            Paths.get("xsl-measures.csv"),
            Paths.get("target"),
            Paths.get("target/eo/6-inference")
        );
    }

    private Transpilation transpilation(
        final long deadline, final String memory, final Path temp
    ) {
        return new Transpilation(
            new Tracking(false, false),
            false,
            "PhDefault",
            deadline,
            memory,
            temp.resolve("xsl-measures.csv"),
            temp.resolve("target"),
            temp.resolve("inference")
        );
    }

    private static String tested(final Transpilation train) throws IOException {
        return String.join(
            "",
            train.forSource("foo").apply(
                new EoSyntax(
                    String.join(
                        System.lineSeparator(),
                        "[] > foo", "  [] +> works", "    true > @", ""
                    )
                ).parsed()
            ).xpath("//tests/text()")
        );
    }

    private Transpilation transpilation(final Path tables) {
        return new Transpilation(
            new Tracking(false, false),
            false,
            "PhDefault",
            1L,
            "1G",
            Paths.get("xsl-measures.csv"),
            Paths.get("target"),
            tables
        );
    }

    private static XML postDataXml(final Shape shape) throws Exception {
        final XML xmir = new Xsline(
            new TrClasspath<>(
                "/org/eolang/parser/parse/set-locators.xsl",
                "/org/eolang/maven/transpile/set-original-names.xsl",
                "/org/eolang/maven/transpile/classes.xsl",
                "/org/eolang/maven/transpile/attrs.xsl",
                "/org/eolang/maven/transpile/data.xsl"
            ).back()
        ).pass(
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[] > main",
                    "  number > n",
                    "    40-45-00-00-00-00-00-00"
                )
            ).parsed()
        );
        final Node dom = xmir.inner();
        final Element bytes = (Element) XPathFactory.newInstance().newXPath().evaluate(
            "//o[@name='n']/o[@base='Φ.bytes']", dom, XPathConstants.NODE
        );
        shape.applyBinding(bytes);
        shape.applyLevel(bytes);
        shape.applyValue(
            (Node) XPathFactory.newInstance().newXPath().evaluate(
                "//o[@name='n']", dom, XPathConstants.NODE
            )
        );
        return new XMLDocument(dom);
    }

    private static String javaFromPostData(final XML xmir) throws IOException {
        return new Xsline(
            new TrClasspath<>(
                "/org/eolang/maven/transpile/to-java.xsl"
            ).back()
        ).pass(xmir).xpath("//class/java/text()").get(0);
    }

    private static Stream<Shape> postDataShapes() {
        return Stream.of(
            new Shape(
                "positional alpha zero is safe", "α0", Set.of(Feature.SHORTCUT)
            ),
            new Shape("a bare zero is a named binding", "0", Set.of()),
            new Shape(
                "an absent positional binding is safe", "",
                Set.of(Feature.SHORTCUT)
            ),
            new Shape("phi binding is safe", "φ", Set.of(Feature.SHORTCUT)),
            new Shape("alpha one is unsafe", "α1", Set.of()),
            new Shape("a named binding is unsafe", "wrong", Set.of()),
            new Shape(
                "a level-marked argument is unsafe", "α0", Set.of(Feature.LEVEL)
            ),
            new Shape(
                "an outer value is unsafe", "α0", Set.of(Feature.VALUE)
            )
        );
    }

    private static String attribute(final String java, final String name) {
        final String marker = String.format("this.add(\"%s\"", name);
        final int start = java.indexOf(marker);
        int end = java.indexOf("this.add(\"", start + marker.length());
        if (end < 0) {
            end = java.length();
        }
        return java.substring(start, end);
    }

    /**
     * A post-data fixture feature.
     *
     * @since 0.1
     */
    private enum Feature {

        /** Level marker. */
        LEVEL,
        /** Outer value. */
        VALUE,
        /** Expected literal shortcut. */
        SHORTCUT
    }

    /**
     * One post-data object shape.
     *
     * @param description Description
     * @param binding Binding value
     * @param features Shape features
     * @since 0.1
     */
    private record Shape(
        String description, String binding, Set<Feature> features
    ) {

        String bindingSelector() {
            final String predicate;
            if (this.binding.isEmpty()) {
                predicate = "not(@as)";
            } else {
                predicate = String.format("@as='%s'", this.binding);
            }
            return String.format(
                "//o[@name='n']/o[@base='Φ.bytes' and %s]", predicate
            );
        }

        String levelSelector() {
            final String predicate;
            if (this.features.contains(Feature.LEVEL)) {
                predicate = "@level='1'";
            } else {
                predicate = "not(@level)";
            }
            return String.format(
                "//o[@name='n']/o[@base='Φ.bytes' and %s]", predicate
            );
        }

        String valueCount() {
            final String count;
            if (this.features.contains(Feature.VALUE)) {
                count = "1";
            } else {
                count = "0";
            }
            return count;
        }

        boolean shortcutMatches(final String java) {
            final boolean matches;
            if (this.features.contains(Feature.SHORTCUT)) {
                matches = java.contains(TranspilationTest.literal());
            } else {
                matches = !java.contains(TranspilationTest.literal());
            }
            return matches;
        }

        boolean zeroBindingMatches(final String java) {
            boolean matches = true;
            if ("0".equals(this.binding)) {
                matches = java.contains("new Bind(\"0\",");
            }
            return matches;
        }

        void applyBinding(final Element element) {
            if (this.binding.isEmpty()) {
                element.removeAttribute("as");
            } else {
                element.setAttribute("as", this.binding);
            }
        }

        void applyLevel(final Element element) {
            if (this.features.contains(Feature.LEVEL)) {
                element.setAttribute("level", "1");
            } else {
                element.removeAttribute("level");
            }
        }

        void applyValue(final Node outer) {
            if (this.features.contains(Feature.VALUE)) {
                final Element data = outer.getOwnerDocument().createElement("value");
                data.setTextContent("new byte[] {(byte) 0x01}");
                outer.appendChild(data);
            }
        }
    }
}
