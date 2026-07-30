/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.StEnvelope;

/**
 * The reverse of {@code const-to-dataized.xsl}: folds a printed
 * {@code Φ.dataized(...).as-bytes} wrapper back into a {@code @const}
 * attribute on the original argument.
 * @since 0.62.0
 */
final class DataizedToConst extends StEnvelope {

    /**
     * Ctor.
     */
    DataizedToConst() {
        this(new StClasspath("/org/eolang/printer/print/dataized-to-const.xsl"));
    }

    /**
     * Ctor.
     * @param origin Original shift
     */
    DataizedToConst(final Shift origin) {
        super(origin);
    }
}
