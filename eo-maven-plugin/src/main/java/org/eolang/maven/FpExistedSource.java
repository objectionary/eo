/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

/**
 * Footprint throws exception if source file does not exist.
 * @since 0.41
 */
final class FpExistedSource extends FpEnvelope {

    /**
     * Ctor.
     * @param footprint Original footprint
     */
    FpExistedSource(final Footprint footprint) {
        super(
            (source, target) -> {
                if (!source.toFile().exists()) {
                    throw new IllegalStateException(
                        String.format("Source file %s does not exist", source)
                    );
                }
                return footprint.apply(source, target);
            }
        );
    }
}
