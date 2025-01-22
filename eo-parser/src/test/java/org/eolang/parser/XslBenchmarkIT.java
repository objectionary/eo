/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
package org.eolang.parser;

import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.StClasspath;
import java.io.IOException;
import java.util.concurrent.TimeUnit;
import org.cactoos.io.ResourceOf;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.openjdk.jmh.Main;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;

/**
 * Benchmark for XSL transformations.
 * @since 0.41
 * @checkstyle DesignForExtensionCheck (100 lines)
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@State(Scope.Benchmark)
@Disabled
@SuppressWarnings({
    "JTCOP.RuleAllTestsHaveProductionClass",
    "PMD.JUnitTestClassShouldBeFinal",
    "PMD.JUnit5TestShouldBePackagePrivate"
})
public class XslBenchmarkIT {
    /**
     * Pairs of XSL and worst XMIR for the XSL.
     */
    @Param({
        "/org/eolang/parser/shake/add-default-package.xsl|org/eolang/parser/benchmark/native.xmir",
        "/org/eolang/parser/shake/build-fqns.xsl|org/eolang/parser/benchmark/native.xmir",
        "/org/eolang/parser/shake/explicit-data.xsl|org/eolang/parser/benchmark/native.xmir"
    })
    private String pairs;

    @Benchmark
    public void apply() throws Exception {
        final String[] pair = this.pairs.split("\\|");
        new StClasspath(pair[0]).apply(
            1, new XMLDocument(new ResourceOf(pair[1]).stream())
        );
    }

    @Test
    @SuppressWarnings("JTCOP.LineHitterRule")
    void measuresXslTransformations() throws IOException {
        Main.main(new String[0]);
        Assertions.assertTrue(true, "Benchmark should executed");
    }
}
