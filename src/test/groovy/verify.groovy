/**
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/**
 * Entry point for running validation scripts.
 * To add new validation create new script in this folder and add it
 * to the list below.
 */

[
    'src/test/groovy/check-xsl-id.groovy',
    'src/test/groovy/check-xsl-version.groovy',
].each { filename ->
    evaluate(new File(filename))
    log.info "Verified with $filename - OK"
}
