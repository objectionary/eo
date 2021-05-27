package org.eolang;

import org.eolang.core.EOObject;
import org.eolang.core.data.EODataObject;
import org.eolang.txt.EOsprintf;
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
    void EOisEmpty() {
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
    void EOlength() {
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
    void EOget() {
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
    void EOappend() {
        EOarray array = new EOarray(
                new EODataObject(1),
                new EODataObject(3),
                new EODataObject(5),
                new EODataObject(7),
                new EODataObject(9)
        );
        Long arraySize = array.EOlength()._getData().toInt();
        EOarray appendedArray = array.EOappend(new EODataObject(10));
        MatcherAssert.assertThat(
                appendedArray.EOlength()._getData().toInt(),
                Matchers.equalTo(arraySize+1));
        for(int i = 0; i < arraySize; ++i){
            MatcherAssert.assertThat(
                    appendedArray.EOget(new EODataObject(i))._getData().toInt(),
                    Matchers.equalTo(array.EOget(new EODataObject(i))._getData().toInt()));
        }
        MatcherAssert.assertThat(
                appendedArray.EOget(new EODataObject(arraySize))._getData().toInt(),
                Matchers.equalTo(10L)
        );

    }

    /***
     * Test for {@code EOappend}
     * checks if an elements successfully appends to an array
     */
    @Test
    void EOappendAll() {
        EOarray array = new EOarray(
                new EODataObject(1),
                new EODataObject(3),
                new EODataObject(5),
                new EODataObject(7),
                new EODataObject(9)
        );
        Long arraySize = array.EOlength()._getData().toInt();
        EOarray array2 = new EOarray(
                new EODataObject(2),
                new EODataObject(4),
                new EODataObject(6),
                new EODataObject(8),
                new EODataObject(10),
                new EODataObject(12)
        );
        Long array2Size = array2.EOlength()._getData().toInt();
        //Append not an EOarray
        EOarray appendedArray1 = array.EOappendAll(new EODataObject(10));
        MatcherAssert.assertThat(
                appendedArray1.EOlength()._getData().toInt(),
                Matchers.equalTo(arraySize+1));
        for(int i = 0; i < arraySize; ++i){
            MatcherAssert.assertThat(
                    appendedArray1.EOget(new EODataObject(i))._getData().toInt(),
                    Matchers.equalTo(array.EOget(new EODataObject(i))._getData().toInt()));
        }
        MatcherAssert.assertThat(
                appendedArray1.EOget(new EODataObject(arraySize))._getData().toInt(),
                Matchers.equalTo(10L)
        );

        //Append an empty EOarray
        EOarray appendedArray2 = array.EOappendAll(new EOarray());
        MatcherAssert.assertThat(
                appendedArray2.EOlength()._getData().toInt(),
                Matchers.equalTo(arraySize));
        for(int i = 0; i < arraySize; ++i){
            MatcherAssert.assertThat(
                    appendedArray2.EOget(new EODataObject(i))._getData().toInt(),
                    Matchers.equalTo(array.EOget(new EODataObject(i))._getData().toInt()));
        }

        //Append an not empty EOarray
        EOarray appendedArray3 = array.EOappendAll(array2);
        MatcherAssert.assertThat(
                appendedArray3.EOlength()._getData().toInt(),
                Matchers.equalTo(arraySize+array2Size));
        for(Long i = 0L; i < arraySize; ++i){
            MatcherAssert.assertThat(
                    appendedArray3.EOget(new EODataObject(i))._getData().toInt(),
                    Matchers.equalTo(array.EOget(new EODataObject(i))._getData().toInt()));
        }
        for(Long i = arraySize; i < arraySize+array2Size; ++i){
            MatcherAssert.assertThat(
                    appendedArray3.EOget(new EODataObject(i))._getData().toInt(),
                    Matchers.equalTo(array2.EOget(new EODataObject(i - arraySize))._getData().toInt()));
        }

    }

    /***
     * Checks that {@code EOreduce} is able to reduce an int array to a sum of its elements.
     */
    @Test
    void EOreduceSumsIntArray() {
        EOarray inputArray = new EOarray(
                new EOint(1),
                new EOint(3),
                new EOint(5),
                new EOint(7),
                new EOint(9)
        );
        Long expectedResult = 25L;
        EOObject reducerObject = new EOObject() {
            public EOObject EOreduce(EOint subtotal, EOObject element) {
                return new EOObject() {
                    @Override
                    protected EOObject _decoratee() {
                        // adds the current element of the array to the subtotal
                        return subtotal.EOadd(element);
                    }
                };
            }
        };
        EOint initialAccumulator = new EOint(0);
        EOObject reducedValue = inputArray.EOreduce(initialAccumulator, reducerObject);

        MatcherAssert.assertThat(reducedValue._getData().toInt(), Matchers.equalTo(expectedResult));
    }

    /***
     * Checks that {@code EOmap} is able to map an int array to an array of squares of its elements.
     */
    @Test
    void EOmapTransformsIntArrayToItsSquares() {
        EOarray inputArray = new EOarray(
                new EOint(1),
                new EOint(3),
                new EOint(5),
                new EOint(7),
                new EOint(9)
        );
        EOarray expectedResultArray = new EOarray(
                new EOint(1),
                new EOint(9),
                new EOint(25),
                new EOint(49),
                new EOint(81)
        );
        EOObject mapperObject = new EOObject() {
            public EOObject EOmap(EOint element) {
                return new EOObject() {
                    @Override
                    protected EOObject _decoratee() {
                        return element.EOpow(new EOint(2));
                    }
                };
            }
        };
        EOarray resultArray = inputArray.EOmap(mapperObject);
        MatcherAssert.assertThat(resultArray, Matchers.is(expectedResultArray));
    }

    /***
     * Checks that {@code EOmapi} is able to map a string array
     * to an array of strings with indices concatenated to its elements.
     */
    @Test
    void EOmapiTransformsStringArrayUsingIndices() {
        EOarray inputArray = new EOarray(
                new EOstring("this"),
                new EOstring("is"),
                new EOstring("a"),
                new EOstring("test"),
                new EOstring("strings array")
        );
        EOarray expectedResultArray = new EOarray(
                new EOstring("this0"),
                new EOstring("is1"),
                new EOstring("a2"),
                new EOstring("test3"),
                new EOstring("strings array4")
        );
        EOObject mapperObject = new EOObject() {
            public EOObject EOmapi(EOstring element, EOint index) {
                return new EOObject() {
                    @Override
                    protected EOObject _decoratee() {
                        return new EOsprintf(
                                    new EOstring("%s%d"),
                                    element,
                                    index
                        );
                    }
                };
            }
        };
        EOarray resultArray = inputArray.EOmapi(mapperObject);
        MatcherAssert.assertThat(resultArray, Matchers.is(expectedResultArray));
    }

}