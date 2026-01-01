/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package benchmarks;

import com.jcabi.xml.XML;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.Xsline;
import fixtures.LargeXmir;
import java.util.concurrent.TimeUnit;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;

/**
 * Benchmark for XSL transformations.
 *
 * @since 0.41
 * @checkstyle NonStaticMethodCheck (100 lines)
 */
@Fork(1)
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleCorrectTestName"})
public class XslBench {

    /**
     * Large XMIR document.
     */
    private static final XML INPUT = new LargeXmir().xml();

    /**
     * All sheets to use.
     */
    private static final Xsline LINE = new Xsline(
        new TrClasspath<>(
            "/org/eolang/parser/parse/move-voids-up.xsl",
            "/org/eolang/parser/parse/validate-before-stars.xsl",
            "/org/eolang/parser/parse/resolve-before-star.xsl",
            "/org/eolang/parser/parse/wrap-method-calls.xsl",
            "/org/eolang/parser/parse/const-to-dataized.xsl",
            "/org/eolang/parser/parse/stars-to-tuples.xsl",
            "/org/eolang/parser/shake/add-default-package.xsl",
            "/org/eolang/parser/shake/build-fqns.xsl"
        ).back()
    );

    @Benchmark
    public final void manySheetsOnLargeXmir() {
        XslBench.LINE.pass(XslBench.INPUT);
    }
}
