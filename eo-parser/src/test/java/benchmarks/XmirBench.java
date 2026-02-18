/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package benchmarks;

import com.jcabi.xml.XML;
import fixtures.LargeXmir;
import java.util.concurrent.TimeUnit;
import org.eolang.parser.Xmir;
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
 * Benchmark for XMIR to EO transformations.
 * To run benchmarks using Maven, use the command:
 * {@code mvn jmh:benchmark}
 *
 * @since 0.41
 * @checkstyle DesignForExtensionCheck (100 lines)
 * @checkstyle NonStaticMethodCheck (100 lines)
 */
@Fork(1)
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleCorrectTestName"})
public class XmirBench {

    /**
     * Large XMIR document.
     */
    private static final XML XMIR = new LargeXmir("noname", "com/sun/jna/Klass.class").xml();

    @Benchmark
    public void xmirToEO() {
        new Xmir(XmirBench.XMIR).toEO();
    }
}
