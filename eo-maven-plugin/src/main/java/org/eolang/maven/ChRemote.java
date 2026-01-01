/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import org.cactoos.Text;

/**
 * Hash of tag from objectionary.
 *
 * @since 0.26
 */
final class ChRemote implements CommitHash {

    /**
     * Cached text of hashes.
     */
    private static final Text CACHE = new CommitHashesText();

    /**
     * Count of retries possible.
     */
    private static final Integer RETRIES = 3;

    /**
     * Tag.
     */
    private final String tag;

    /**
     * Constructor.
     *
     * @param tag Tag
     */
    ChRemote(final String tag) {
        this.tag = tag;
    }

    @Override
    public String value() {
        try {
            final String sha = new ChText(
                ChRemote.CACHE::asString,
                this.tag,
                ChRemote.RETRIES
            ).value();
            Logger.debug(this, "Git sha of %s is %s", this.tag, sha);
            return sha;
        } catch (final ChText.NotFound ex) {
            throw new IllegalArgumentException(
                String.format(
                    "Tag '%s' doesn't exist or the list of all tags was not loaded correctly",
                    this.tag
                ),
                ex
            );
        }
    }
}
