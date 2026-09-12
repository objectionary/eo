/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XMLDocument;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Settled}.
 *
 * @since 0.73.0
 */
final class SettledTest {

    @Test
    @SuppressWarnings("PMD.UnitTestContainsTooManyAsserts")
    void settlesAndOrdersAChain() {
        final String xml = String.join(
            "", "<provides><type id='Φ.app.root'><attr name='step0' type='Φ.target.0'/></type>",
            "<type id='Φ.target.0'><attr name='step1' type='Φ.target.1'/></type>",
            "<type id='Φ.target.1'><attr name='step2' type='Φ.target.2'/></type>",
            "<type id='Φ.target.2'><attr name='step3' type='Φ.target.3'/></type>",
            "<type id='Φ.target.3'/></provides>"
        );
        final List<Site> sites = List.of(
            this.site("Φ.app.d0", "step0", "Φ.app.root"),
            this.site("Φ.app.d1", "step1", "Φ.app.d0"),
            this.site("Φ.app.d2", "step2", "Φ.app.d1"),
            this.site("Φ.app.d3", "step3", "Φ.app.d2")
        );
        final Map<String, String> expected = new LinkedHashMap<>(0);
        expected.put("Φ.app.d0", "Φ.target.0");
        expected.put("Φ.app.d1", "Φ.target.1");
        expected.put("Φ.app.d2", "Φ.target.2");
        expected.put("Φ.app.d3", "Φ.target.3");
        MatcherAssert.assertThat(
            "chain follows source order",
            this.dispatched(sites, xml).answers(new LinkedHashMap<>(0)),
            Matchers.equalTo(expected)
        );
        final List<Site> reversed = new ArrayList<>(sites);
        Collections.reverse(reversed);
        MatcherAssert.assertThat(
            "reversed chain is retried in dependency order",
            this.dispatched(reversed, xml).answers(new LinkedHashMap<>(0)),
            Matchers.equalTo(expected)
        );
    }

    private Site site(final String made, final String step, final String bearer) {
        return new Site(
            new Xnav(
                new XMLDocument(
                    String.format(
                        "<o base='.%s' loc='%s'><o loc='%s'/></o>", step, made, bearer
                    )
                ).inner()
            ).element("o")
        );
    }

    private Dispatched dispatched(final List<Site> sites, final String xml) {
        return new Dispatched(
            new XMLDocument(xml), sites, Collections.emptyMap(),
            Collections.emptyMap(), Collections.emptyMap(), Collections.emptyList()
        );
    }
}
