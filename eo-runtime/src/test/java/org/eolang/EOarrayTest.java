package org.eolang;

import org.eolang.core.EOObject;
import org.eolang.core.data.EODataObject;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;


/**
 * Test cases for {@link EOarray}
 */
class EOarrayTest {

    /***
     * Test for {@code EOisEmpty}
     * checks if an array is empty
     */
    @Test
    void isEmpty() {
        EOarray array = new EOarray();
        MatcherAssert.assertThat(
                array.EOisEmpty()._getData().toBoolean(),
                Matchers.equalTo(true)
        );

    }

    /***
     * Test for {@code EOlength}
     * checks if the length of the array is returned
     */
    @Test
    void length() {
        EOarray array = new EOarray(
                new EODataObject(1),
                new EODataObject(3),
                new EODataObject(5),
                new EODataObject(7),
                new EODataObject(9)
        );
        MatcherAssert.assertThat(
                array.EOlength()._getData().toInt(),
                Matchers.equalTo(5L)
        );
    }

    /***
     * Test for {@code EOget}
     * checks if the element at a specified position of the array is returned
     */
    @Test
    void get() {
        EOarray array = new EOarray(
                new EODataObject(1),
                new EODataObject(3),
                new EODataObject(5),
                new EODataObject(7),
                new EODataObject(9)
        );
        MatcherAssert.assertThat(
                array.EOget(
                        new EODataObject(2))._getData().toInt(),
                Matchers.equalTo(5L)
        );
    }

    /***
     * Test for {@code EOappend}
     * checks if an element successfully appends to an array
     */
    @Test
    void append() {
        EOarray array = new EOarray(
                new EODataObject(1),
                new EODataObject(3),
                new EODataObject(5),
                new EODataObject(7),
                new EODataObject(9)
        );
        EOarray appendedArray = array.EOappend(new EODataObject(10));
        MatcherAssert.assertThat(
                appendedArray.EOget(new EODataObject(5))._getData().toInt(),
                Matchers.equalTo(10L)
        );

    }

    /***
     * Test for {@code EOreduce}
     * Checks if the reduction operation returns the correct subtotal/results
     */
    @Test
    void reduce() {
        EOarray array = new EOarray(
                new EODataObject(1),
                new EODataObject(3),
                new EODataObject(5),
                new EODataObject(7),
                new EODataObject(9)
        );
        EOObject a = array.EOreduce(new EODataObject(0), new EOObject() {
            public EOObject EOreduce(EOObject subtotal, EOObject element) {
                return new EODataObject(
                        new EOint(
                                subtotal._getData().toInt()
                        )._getAttribute("EOadd", element)._getData().toInt()
                );
            }
        });
        MatcherAssert.assertThat(a._getData().toInt(), Matchers.equalTo(25L));
    }

    /***
     * Test for {@code EOmap}
     * Checks if the map operation correctly maps each element of an array to another value (the square) correctly
     */
    @Test
    void map() {
        EOarray array = new EOarray(
                new EODataObject(1),
                new EODataObject(3),
                new EODataObject(5),
                new EODataObject(7),
                new EODataObject(9)
        );
        EOarray expectedArray = new EOarray(
                new EODataObject(1),
                new EODataObject(9),
                new EODataObject(25),
                new EODataObject(49),
                new EODataObject(81)
        );
        EOarray newArray = array.EOmap(new EOObject() {
            public EOObject EOmap(EOObject element) {
                return new EODataObject(
                        new EOint(
                                element._getData().toInt()
                        )._getAttribute("EOpow", new EODataObject(2))._getData().toInt()
                );
            }
        });
        for(int i=0;i<array.EOlength()._getData().toInt();i++)
            MatcherAssert.assertThat(
                    newArray.EOget(new EODataObject(i))._getData().toInt(),
                    Matchers.equalTo(expectedArray.EOget(new EODataObject(i))._getData().toInt()));
    }

    @Test
    void EOmapi() {
        EOarray array = new EOarray(
                new EODataObject(1),
                new EODataObject(3),
                new EODataObject(5),
                new EODataObject(7),
                new EODataObject(9)
        );
        EOarray expectedArray = new EOarray(
                new EODataObject(1),
                new EODataObject(2),
                new EODataObject(3),
                new EODataObject(4),
                new EODataObject(5)
        );
        EOarray newArray = array.EOmapi(new EOObject() {
            public EOObject EOmapi(EOObject currentElement, EOObject index) {
                return new EODataObject(
                        new EOint(
                                currentElement._getData().toInt()
                        )._getAttribute("EOsub", index)._getData().toInt()
                );
            }
        });
        for(int i=0;i<array.EOlength()._getData().toInt();i++)
            MatcherAssert.assertThat(
                    newArray.EOget(new EODataObject(i))._getData().toInt(),
                    Matchers.equalTo(expectedArray.EOget(new EODataObject(i))._getData().toInt()));
    }
}