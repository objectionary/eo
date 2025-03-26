/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrEnvelope;
import com.yegor256.xsline.TrJoined;
import org.eolang.parser.StFlatBytes;
import org.eolang.parser.TrFull;

/**
 * Train of XSL shake shifts.
 *
 * @since 0.1
 */
final class TrShaking extends TrEnvelope {
    /**
     * Ctor.
     */
    TrShaking() {
        super(
            new TrJoined<>(
                new TrDefault<>(new StFlatBytes()),
                new TrFull(
                    new TrClasspath<>(
                        "/org/eolang/maven/shake/cti-adds-errors.xsl",
                        "/org/eolang/maven/shake/add-probes.xsl",
                        "/org/eolang/maven/shake/set-locators.xsl",
                        "/org/eolang/maven/shake/set-original-names.xsl"
                    ).back()
                )
            )
        );
    }
}
