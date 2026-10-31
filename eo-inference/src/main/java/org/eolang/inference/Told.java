/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Map;

/**
 * What the walk tells of each place (design doc 5.1), ready to be written.
 *
 * <p>For each place, every set of labels one copy says there, joined by a
 * tab, with the number of copies that say it; for each formation, what its
 * copies reduce to, the labels joined by a space, with the number of copies;
 * and for each argument, the one void it lands in.</p>
 *
 * @param sets The sets of labels, by place
 * @param reduction What the copies reduce to, by formation
 * @param lands The void each argument lands in, by place
 * @since 0.76.0
 */
record Told(
    Map<String, Map<String, Integer>> sets,
    Map<String, Map<String, Integer>> reduction,
    Map<String, String> lands
) {
}
