/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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

import java.util.HashMap;
import java.util.Map;

/**
 * Stopwatch for performance metrics.
 * NOT thread-safe.
 * <br/>Usage
 * <pre>
 * StopWatch watch = new StopWatch();
 * ...
 * double tag = watch.lap("tag");
 * ...
 * double total = watch.lap();
 * </pre>
 *
 * @since 1.0
 */
public class StopWatch {
    /**
     * Start.
     */
    private final long start;

    /**
     * Laps.
     */
    private final Map<String, Long> laps;

    /**
     * Ctor.
     * Start measurement.
     */
    public StopWatch() {
        this.start = System.nanoTime();
        this.laps = new HashMap<>();
    }

    /**
     * Measure time since start.
     * @param tag Lap tag
     * @return Seconds
     */
    public double lap(final String tag) {
        final long time = System.nanoTime() - this.start;
        this.laps.put(tag, time);
        return StopWatch.seconds(time);
    }

    /**
     * Measure time since start with empty tag.
     * @return Seconds
     */
    public double lap() {
        return this.lap("");
    }

    /**
     * Get duration for lap in seconds.
     * @param tag Lap tag
     * @return Duration
     */
    public double time(final String tag) {
        double duration = 0.0d;
        if (this.laps.containsKey(tag)) {
            duration = StopWatch.seconds(this.laps.get(tag));
        }
        return duration;
    }

    /**
     * Total time, sec.
     * @return Total in seconds
     */
    public double total() {
        return this.laps.values().stream()
            .mapToDouble(StopWatch::seconds)
            .max().orElse(0.0);
    }

    /**
     * Translate nanos to seconds.
     * @param nanos Duration
     * @return Duration in seconds
     */
    private static double seconds(final long nanos) {
        return nanos / 1_000_000_000.0;
    }
}
