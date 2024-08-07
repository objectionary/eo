/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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

import com.jcabi.xml.XMLDocument;
import com.yegor256.Jaxec;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.Xsline;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import org.cactoos.Scalar;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;
import org.cactoos.text.TextOf;
import org.eolang.maven.util.HmBase;
import org.eolang.maven.util.Home;
import org.yaml.snakeyaml.Yaml;

/**
 * Test scenario in YAML.
 *
 * @since 0.40
 * @todo #3267:90min Add better error printing. Ideally, it should print the difference
 *  the way the linux diff command does (line by line).
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleCorrectTestName"})
final class Xmir2Xmir {

    /**
     * Expected content of xml.
     */
    private final Unchecked<String> expected;

    /**
     * Content of xml after transformations.
     */
    private final Unchecked<String> processed;

    /**
     * Ctor.
     * @param pack Content of .yaml test.
     * @param home Where to save files.
     */
    public Xmir2Xmir(final String pack, final Path home) {
        this(
            new Sticky<>(() -> new Yaml().load(pack)),
            new HmBase(home)
        );
    }

    /**
     * Ctor.
     * @param yaml Parsed yaml.
     * @param home Home to save.
     */
    private Xmir2Xmir(final Scalar<Map<String, Object>> yaml, final Home home) {
        this(
            () -> Xmir2Xmir.xcop(
                (String) yaml.value().get("expected"),
                home,
                "expected.xml"
            ),
            () -> Xmir2Xmir.xcop(
                Xmir2Xmir.process(
                    (String) yaml.value().get("before"),
                    (Iterable<String>) yaml.value().get("xsls")
                ),
                home,
                "processed.xml"
            )
        );
    }

    /**
     * Ctor.
     * @param expected Expected normalized xml content.
     * @param processed Processed by transformations xml content.
     */
    private Xmir2Xmir(final Scalar<String> expected, final Scalar<String> processed) {
        this.expected = new Unchecked<>(new Sticky<>(expected));
        this.processed = new Unchecked<>(new Sticky<>(processed));
    }

    @Override
    public String toString() {
        return new StringBuilder().append("Expected and processed xmls sre different.\n")
            .append("Expected:\n")
            .append(this.expected.value())
            .append("\nProcessed:\n")
            .append(this.processed.value()).toString();
    }

    @Override
    public boolean equals(final Object obj) {
        if (!(obj instanceof Boolean)) {
            throw new IllegalArgumentException(
                String.format(
                    "Can't compare with anything except Boolean: %s",
                    obj.getClass()
                )
            );
        }
        return new Unchecked<>(
            () -> this.expected.value().equals(this.processed.value())
        ).value();
    }

    @Override
    public int hashCode() {
        throw new UnsupportedOperationException("#hashCode()");
    }

    /**
     * Convert xmir to a determined format using the command
     *  [xcop](https://github.com/yegor256/xcop).
     * @param raw Raw xmir content.
     * @param home Home.
     * @param dst Filename where to save.
     * @return Xcop'ed xmir content.
     */
    private static String
        xcop(final String raw, final Home home, final String dst) throws Exception {
        final Path relative = Paths.get(dst);
        home.save(raw, relative);
        new Jaxec("xcop", "--fix")
            .with(dst)
            .withHome(home.absolute(Paths.get(".")))
            .execUnsafe();
        return new TextOf(home.absolute(relative)).asString();
    }

    /**
     * Pass xml through xsls.
     * @param before XML content before.
     * @param xsls Xsls.
     * @return Content of output xml.
     */
    private static String process(final String before, final Iterable<String> xsls) {
        TrClasspath<Shift> train = new TrClasspath<>();
        for (final String sheet : xsls) {
            train = train.with(sheet);
        }
        return new Xsline(train.back()).pass(new XMLDocument(before)).toString();
    }
}
