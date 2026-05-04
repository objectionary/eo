/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.manifests.Manifests;
import org.cactoos.Scalar;

/**
 * Offline runtime dependency from the current project manifest.
 * @since 0.62.0
 */
final class RtOffline implements Scalar<Dep> {

    /**
     * EO current offline version.
     */
    private static final Dep EO_OFFLINE = new Dep().withGroupId("org.eolang")
        .withArtifactId("eo-runtime")
        .withVersion(Manifests.read("EO-Version"));

    @Override
    public Dep value() throws Exception {
        return RtOffline.EO_OFFLINE;
    }
}
