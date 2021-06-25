package org.eolang;

import org.eolang.hse.EOarray;
import org.eolang.hse.EOint;
import org.eolang.hse.EOstring;
import org.eolang.hse.EOtuple;
import org.eolang.hse.core.EOObject;
import org.eolang.hse.io.EOstdout;
import org.eolang.hse.txt.EOsprintf;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import static net.obvj.junit.utils.matchers.AdvancedMatchers.throwsException;
import static org.hamcrest.CoreMatchers.is;

/**
 * Test cases for {@link EOarray}.
 */
class EOarrayTest {

    /**
     * Test for {@code EOappend}
     * checks if an elements successfully appends to an array
     */
    @Test
    void EOappendAll() {
        EOarray array = new EOarray(
                new EOint(1),
                new EOint(3),
                new EOint(5),
                new EOint(7),
                new EOint(9)
        );
        Long arraySize = array.EOlength()._getData().toInt();
        EOarray array2 = new EOarray(
                new EOint(2),
                new EOint(4),
                new EOint(6),
                new EOint(8),
                new EOint(10),
                new EOint(12)
        );
        Long array2Size = array2.EOlength()._getData().toInt();
        //Append not an EOarray
        EOarray appendedArray1 = array.EOappendAll(new EOint(10));
        MatcherAssert.assertThat(
                appendedArray1.EOlength()._getData().toInt(),
                is(arraySize + 1));
        for (int i = 0; i < arraySize; ++i) {
            MatcherAssert.assertThat(
                    appendedArray1.EOget(new EOint(i))._getData().toInt(),
                    is(array.EOget(new EOint(i))._getData().toInt()));
        }
        MatcherAssert.assertThat(
                appendedArray1.EOget(new EOint(arraySize))._getData().toInt(),
                is(10L)
        );

        //Append an empty EOarray
        EOarray appendedArray2 = array.EOappendAll(new EOarray());
        MatcherAssert.assertThat(
                appendedArray2.EOlength()._getData().toInt(),
                is(arraySize));
        for (int i = 0; i < arraySize; ++i) {
            MatcherAssert.assertThat(
                    appendedArray2.EOget(new EOint(i))._getData().toInt(),
                    is(array.EOget(new EOint(i))._getData().toInt()));
        }

        //Append an not empty EOarray
        EOarray appendedArray3 = array.EOappendAll(array2);
        MatcherAssert.assertThat(
                appendedArray3.EOlength()._getData().toInt(),
                is(arraySize + array2Size));
        for (Long i = 0L; i < arraySize; ++i) {
            MatcherAssert.assertThat(
                    appendedArray3.EOget(new EOint(i))._getData().toInt(),
                    is(array.EOget(new EOint(i))._getData().toInt()));
        }
        for (Long i = arraySize; i < arraySize + array2Size; ++i) {
            MatcherAssert.assertThat(
                    appendedArray3.EOget(new EOint(i))._getData().toInt(),
                    is(array2.EOget(new EOint(i - arraySize))._getData().toInt()));
        }
    }

    /**
     * Checks that {@code EOappend} is able to add a new element to the end of an empty array.
     */
    @Test
    void EOappendWorksWithEmptyArrays() {
        EOarray inputArray = new EOarray();
        EOObject appendedElement = new EOint(10);
        EOarray expectedResultArray = new EOarray(appendedElement);
        EOarray resultArray = inputArray.EOappend(appendedElement);

        MatcherAssert.assertThat(resultArray, is(expectedResultArray));
    }

    /**
     * Checks that {@code EOappend} is able to add a new element to the end of a non-empty array.
     */
    @Test
    void EOappendWorksWithNonEmptyArrays() {
        EOarray inputArray = new EOarray(
                new EOint(1),
                new EOint(3),
                new EOint(5),
                new EOint(7),
                new EOint(9)
        );
        EOObject appendedElement = new EOint(10);
        EOarray expectedResultArray = new EOarray(
                new EOint(1),
                new EOint(3),
                new EOint(5),
                new EOint(7),
                new EOint(9),
                appendedElement
        );
        EOarray resultArray = inputArray.EOappend(appendedElement);

        MatcherAssert.assertThat(resultArray, is(expectedResultArray));
    }

    /**
     * Checks that {@code EOeach} is able to dataize elements of a non-empty array.
     * To do this, the test performs {@code EOeach} on an array of strings trying to print its elements,
     * and catches the stdout stream.
     */
    @Test
    void EOeachDataizesElementsOfNonEmptyArray() {
        PrintStream systemStdout = System.out;
        try {
            EOarray inputArray = new EOarray(
                    new EOstring("this "),
                    new EOstring("is "),
                    new EOstring("text "),
                    new EOstring("to be printed!")
            );
            String expectedStdout = "this is text to be printed!";
            EOObject evaluatorObject = new EOObject() {
                public EOObject EOeach(EOObject element) {
                    return new EOObject() {
                        @Override
                        protected EOObject _decoratee() {
                            // prints the current element
                            return new EOstdout(element);
                        }
                    };
                }
            };
            // mocking the stdout stream
            ByteArrayOutputStream stdout = new ByteArrayOutputStream();
            StdoutMockingUtils.mockSystemOut(stdout);
            // performing the each operation on all the elements of the array
            EOObject returnedValue = inputArray.EOeach(evaluatorObject);

            // EOeach should have dataized the elements, thus the stdout must have been changed
            MatcherAssert.assertThat(stdout.toString(), is(expectedStdout));
            // EOeach always returns true
            MatcherAssert.assertThat(returnedValue._getData().toBoolean(), is(true));
        } finally {
            // rollback the stdout to the original system stream
            StdoutMockingUtils.rollbackChangesToStdout(systemStdout);
        }
    }

    /**
     * Checks that {@code EOeach} does not dataize anything when working with empty arrays.
     * To do this, the test performs {@code EOeach} on an empty array trying to print its elements,
     * and catches the stdout stream.
     */
    @Test
    void EOeachDoesNotDataizeElementsOfEmptyArray() {
        PrintStream systemStdout = System.out;
        try {
            EOarray inputArray = new EOarray();
            String expectedStdout = "";
            EOObject evaluatorObject = new EOObject() {
                public EOObject EOeach(EOObject element) {
                    return new EOObject() {
                        @Override
                        protected EOObject _decoratee() {
                            // prints the current element
                            return new EOstdout(element);
                        }
                    };
                }
            };
            // mocking the stdout stream
            ByteArrayOutputStream stdout = new ByteArrayOutputStream();
            StdoutMockingUtils.mockSystemOut(stdout);
            // performing the each operation on all the elements of the array
            EOObject returnedValue = inputArray.EOeach(evaluatorObject);

            // EOeach should not have dataized the elements, thus the stdout must not have been changed
            MatcherAssert.assertThat(stdout.toString(), is(expectedStdout));
            // EOeach always returns true
            MatcherAssert.assertThat(returnedValue._getData().toBoolean(), is(true));
        } finally {
            // rollback the stdout to the original system stream
            StdoutMockingUtils.rollbackChangesToStdout(systemStdout);
        }
    }

    /**
     * Checks that {@code EOget} fails when working with empty arrays.
     */
    @Test
    void EOgetFailsWithEmptyArrays() {
        EOarray inputArray = new EOarray();
        EOint index = new EOint(3);
        MatcherAssert.assertThat(
                () -> inputArray.EOget(index),
                throwsException(IndexOutOfBoundsException.class)
                        .withMessage("Cannot retrieve the element at the position 3 of the following array: array([]). The index is out of bounds.")
        );
    }

    /**
     * Checks that {@code EOget} fails when the index is less than zero.
     */
    @Test
    void EOgetFailsWithNegativeIndex() {
        EOarray inputArray = new EOarray(
                new EOint(1),
                new EOint(3),
                new EOint(5),
                new EOint(9)
        );
        EOint index = new EOint(-1);
        MatcherAssert.assertThat(
                () -> inputArray.EOget(index),
                throwsException(IndexOutOfBoundsException.class)
                        .withMessage("Cannot retrieve the element at the position -1 of the following array: array([int(1), int(3), int(5), int(9)]). The index is out of bounds.")
        );
    }

    /**
     * Checks that {@code EOget} fails when the index is larger than the highest index of an array.
     */
    @Test
    void EOgetFailsWithTooLargeIndex() {
        EOarray inputArray = new EOarray(
                new EOint(1),
                new EOint(3),
                new EOint(5),
                new EOint(9)
        );
        EOint index = new EOint(4);
        MatcherAssert.assertThat(
                () -> inputArray.EOget(index),
                throwsException(IndexOutOfBoundsException.class)
                        .withMessage("Cannot retrieve the element at the position 4 of the following array: array([int(1), int(3), int(5), int(9)]). The index is out of bounds.")
        );
    }

    /**
     * Checks that {@code EOget} is able to retrieve an element from a non-empty array.
     */
    @Test
    void EOgetWorksWithNonEmptyArrays() {
        EOint expectedResult = new EOint(7);
        EOarray inputArray = new EOarray(
                new EOint(1),
                new EOint(3),
                new EOint(5),
                expectedResult,
                new EOint(9)
        );
        EOint index = new EOint(3);
        EOint result = (EOint) inputArray.EOget(index);
        MatcherAssert.assertThat(result, is(expectedResult));
    }

    /**
     * Checks that {@code EOisEmpty} is able to assess that an empty array is empty.
     */
    @Test
    void EOisEmptyForEmptyArrays() {
        EOarray array = new EOarray();
        MatcherAssert.assertThat(array.EOisEmpty()._getData().toBoolean(), is(true));
    }

    /**
     * Checks that {@code EOisEmpty} is able to assess that a non-empty array is not empty.
     */
    @Test
    void EOisEmptyForNonEmptyArrays() {
        EOarray array = new EOarray(new EOint(1));
        MatcherAssert.assertThat(array.EOisEmpty()._getData().toBoolean(), is(false));
    }

    /**
     * Checks that {@code EOlength} returns 0 for an empty array.
     */
    @Test
    void EOlengthForEmptyArrays() {
        EOarray array = new EOarray();
        Long expectedResult = 0L;
        MatcherAssert.assertThat(array.EOlength()._getData().toInt(), is(expectedResult));
    }

    /**
     * Checks that {@code EOlength} returns 5 for an array with five elements.
     */
    @Test
    void EOlengthForFiveElementArrays() {
        EOarray array = new EOarray(
                new EOstring("test"),
                new EOstring("test"),
                new EOstring("test"),
                new EOstring("test"),
                new EOstring("another string")
        );
        Long expectedResult = 5L;
        MatcherAssert.assertThat(array.EOlength()._getData().toInt(), is(expectedResult));
    }

    /**
     * Checks that {@code EOlength} returns 1 for an array with one element.
     */
    @Test
    void EOlengthForOneElementArrays() {
        EOarray array = new EOarray(new EOstring("test"));
        Long expectedResult = 1L;
        MatcherAssert.assertThat(array.EOlength()._getData().toInt(), is(expectedResult));
    }

    /**
     * Checks that {@code EOmap} is able to map a non-empty int array to an array of squares of its elements.
     */
    @Test
    void EOmapTransformsNonEmptyIntArrayToItsSquares() {
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
        MatcherAssert.assertThat(resultArray, is(expectedResultArray));
    }

    /**
     * Checks that {@code EOmap} is able to map an empty array to another empty array.
     */
    @Test
    void EOmapWorksWithEmptyArrays() {
        EOarray inputArray = new EOarray();
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
        MatcherAssert.assertThat(resultArray.EOisEmpty()._getData().toBoolean(), is(true));
    }

    /**
     * Checks that {@code EOmapi} is able to map a non-empty string array
     * to an array of strings with indices concatenated to its elements.
     */
    @Test
    void EOmapiTransformsNonEmptyStringArrayUsingIndices() {
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
        MatcherAssert.assertThat(resultArray, is(expectedResultArray));
    }

    /**
     * Checks that {@code EOmapi} is able to map an empty array to another empty array.
     */
    @Test
    void EOmapiWorksWithEmptyArrays() {
        EOarray inputArray = new EOarray();
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
        MatcherAssert.assertThat(resultArray.EOisEmpty()._getData().toBoolean(), is(true));
    }

    /**
     * Checks that {@code EOpairs} does not guarantee uniqueness of pairs when elements of an array are not unique.
     */
    @Test
    void EOpairsDoesNotGuaranteeUniqueness() {
        EOarray inputArray = new EOarray(
                new EOint(1),
                new EOint(2),
                new EOint(2),
                new EOint(4)
        );
        EOarray expectedResultArray = new EOarray(
                new EOtuple(new EOint(1), new EOint(2)),
                new EOtuple(new EOint(1), new EOint(2)),
                new EOtuple(new EOint(1), new EOint(4)),
                new EOtuple(new EOint(2), new EOint(2)),
                new EOtuple(new EOint(2), new EOint(4)),
                new EOtuple(new EOint(2), new EOint(4))
        );
        EOarray resultArray = inputArray.EOpairs();
        MatcherAssert.assertThat(resultArray, is(expectedResultArray));
    }

    /**
     * Checks that {@code EOpairs} is able to produce 2-Combinations when the elements of the array are unique.
     */
    @Test
    void EOpairsProduces2Combinations() {
        EOarray inputArray = new EOarray(
                new EOint(1),
                new EOint(2),
                new EOint(3),
                new EOint(4)
        );
        EOarray expectedResultArray = new EOarray(
                new EOtuple(new EOint(1), new EOint(2)),
                new EOtuple(new EOint(1), new EOint(3)),
                new EOtuple(new EOint(1), new EOint(4)),
                new EOtuple(new EOint(2), new EOint(3)),
                new EOtuple(new EOint(2), new EOint(4)),
                new EOtuple(new EOint(3), new EOint(4))
        );
        EOarray resultArray = inputArray.EOpairs();
        MatcherAssert.assertThat(resultArray, is(expectedResultArray));
    }

    /**
     * Checks that {@code EOpairs} returns empty arrays when working with empty arrays.
     */
    @Test
    void EOpairsWorksWithEmptyArrays() {
        EOarray inputArray = new EOarray();
        EOarray resultArray = inputArray.EOpairs();
        MatcherAssert.assertThat(resultArray.EOisEmpty()._getData().toBoolean(), is(true));
    }

    /**
     * Checks that {@code EOreduce} is able to reduce a non-empty int array to a sum of its elements.
     */
    @Test
    void EOreduceSumsNonEmptyIntArray() {
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

        MatcherAssert.assertThat(reducedValue._getData().toInt(), is(expectedResult));
    }

    /**
     * Checks that {@code EOreduce} return the initial value when working with empty arrays.
     */
    @Test
    void EOreduceWorksWithEmptyArrays() {
        EOarray inputArray = new EOarray();
        Long expectedResult = 0L;
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

        MatcherAssert.assertThat(reducedValue._getData().toInt(), is(expectedResult));
    }

    /**
     * Checks that {@code EOreducei} is able to work with empty arrays. The result must be equal to the initial value.
     */
    @Test
    void EOreduceiWorksWithEmptyIntArray() {
        EOarray inputArray = new EOarray();
        EOObject reducerObject = new EOObject() {
            public EOObject EOreducei(EOint subtotal, EOint element, EOint index) {
                return new EOObject() {
                    @Override
                    protected EOObject _decoratee() {
                        // adds the current element of the array and the current index to the subtotal
                        return subtotal.EOadd(element).EOadd(index);
                    }
                };
            }
        };
        EOint initialAccumulator = new EOint(177);
        EOObject reducedValue = inputArray.EOreducei(initialAccumulator, reducerObject);

        MatcherAssert.assertThat(reducedValue, is(initialAccumulator));
    }

    /**
     * Checks that {@code EOreducei} is able to evaluate a polynomial 5*x^4 - 7*x^2 + 20*x + 1, where x = 10.
     * This problem is tested by reducing the array [1, 20, -7, 0, 5]
     * representing the polynomial coefficients (from the lowest to the highest degree of x).
     * Reduction considers the indices of the coefficients array [0, 1, 2, 3, 4],
     * where each index represents a degree of the corresponding x.
     * Reduction works as follows:
     * 1. The initial value is 0.
     * 2. For each element of the coefficients array,
     * multiply it to x raised into the power of the current index, where x = 10.
     * Hence, the expected result is 49501.
     */
    @Test
    void EOreduceiWorksWithNonEmptyIntArray() {
        EOarray inputArray = new EOarray(
                new EOint(1),
                new EOint(20),
                new EOint(-7),
                new EOint(0),
                new EOint(5)
        );
        Long expectedResult = 49501L;
        EOint xValue = new EOint(10L);
        EOObject reducerObject = new EOObject() {
            public EOObject EOreducei(EOint subtotal, EOint element, EOint index) {
                return new EOObject() {
                    @Override
                    protected EOObject _decoratee() {
                        // + x*(a_i)^(c_i)
                        return subtotal.EOadd(element.EOmul(xValue.EOpow(index)));
                    }
                };
            }
        };
        EOint initialAccumulator = new EOint(0);
        EOObject reducedValue = inputArray.EOreducei(initialAccumulator, reducerObject);

        MatcherAssert.assertThat(reducedValue._getData().toInt(), is(expectedResult));
    }

}

class StdoutMockingUtils {
    public static void mockSystemOut(ByteArrayOutputStream newOut) {
        System.setOut(new PrintStream(newOut));
    }

    public static void rollbackChangesToStdout(PrintStream oldOut) {
        System.setOut(oldOut);
    }
}