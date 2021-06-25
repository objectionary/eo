package org.eolang;

import org.eolang.hse.EObool;
import org.eolang.hse.core.data.EODataObject;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link EObool}
 */
class EOboolTest {

    /***
     * Test for datization
     *  Checks if the data is returned
     */
    @Test
    void _getData() {
        EObool boo = new EObool(true);
        MatcherAssert.assertThat(
                boo._getData().toBoolean(),
                Matchers.equalTo(true)
        );
    }

    /***
     * Test for {@code EOif)}
     * checks if the right/wrong object is returned based on the base bool/condition
     */
    @Test
    void EOif() {
        EObool bool = new EObool(false);
        MatcherAssert.assertThat(
                bool.EOif(
                        new EODataObject(1),
                        new EODataObject((2))
                )._getData().toInt(),
                Matchers.equalTo(2L)
        );
    }

    /***
     * Test for {@code EOnot}
     * checks if a bool gets reversed
     */
    @Test
    void EOnot() {
        EObool bool = new EObool(true);
        MatcherAssert.assertThat(
                bool.EOnot()._getData().toBoolean(),
                Matchers.equalTo(false)
        );
    }

    /***
     * Test for {@code EOand}
     * checks if the correct boolean is returned for logical AND
     */
    @Test
    void EOand() {
        EObool bool = new EObool(true);
        EObool rightBool = new EObool(false);
        MatcherAssert.assertThat(
                bool.EOand(rightBool)._getData().toBoolean(),
                Matchers.equalTo(false)
        );
    }

    /***
     * Test for {@code EOor}
     * checks if the correct boolean is returned for logical OR
     */
    @Test
    void EOor() {
        EObool bool = new EObool(true);
        EObool rightBool = new EObool(false);
        MatcherAssert.assertThat(
                bool.EOor(rightBool)._getData().toBoolean(),
                Matchers.equalTo(true)
        );
    }
}