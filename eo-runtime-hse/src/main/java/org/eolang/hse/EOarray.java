package org.eolang.hse;

import org.eolang.hse.core.EOObject;
import org.paukov.combinatorics3.Generator;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Represents an array data structure.
 */
public class EOarray extends EOObject {

    /**
     * The underlying data structure behind this array is a Java List.
     * Effectively, the {@code _array} is an unmodifiable list since both
     * constructors of this class rely on the {@code List.of()} method.
     */
    private final List<EOObject> _array;

    /**
     * Instantiates an empty array.
     */
    public EOarray() {
        _array = Collections.emptyList();
    }

    /**
     * Instantiates a non-empty array.
     *
     * @param objects contents of the array being instantiated.
     */
    public EOarray(EOObject... objects) {
        _array = Arrays.stream(objects).collect(Collectors.toList());
    }

    /**
     * Appends {@code obj} to the end of this array.
     * <p>
     * This operation does not mutate the original array.
     * Instead, it produces a copy of this array and appends {@code obj} to the end of it.
     *
     * @return a copy of this array with {@code obj} appended as its last element.
     */
    public EOarray EOappend(EOObject obj) {
        EOObject[] newArray = new EOObject[_array.size() + 1];
        System.arraycopy(_array.toArray(), 0, newArray, 0, _array.size());
        newArray[_array.size()] = obj;
        return new EOarray(newArray);
    }

    /**
     * If {@code obj} is an EOarray appends item of {@code obj} to to the end of this array.
     * Otherwise appends {@code obj} to the end of this array.
     * <p>
     * This operation does not mutate the original array.
     * Instead, it produces a copy of this array and appends {@code obj} to the end of it.
     *
     * @return a copy of this array with {@code obj} appended as its last element.
     */
    public EOarray EOappendAll(EOObject obj) {
        try {
            int array2Size = obj._getAttribute("EOlength")._getData().toInt().intValue();
            if (array2Size > 0) {
                EOObject[] newArray;
                newArray = new EOObject[_array.size() + array2Size];
                System.arraycopy(_array.toArray(), 0, newArray, 0, _array.size());
                for (int i = _array.size(); i < newArray.length; ++i) {
                    newArray[i] = obj._getAttribute("EOget", new EOint(i - _array.size()));
                }
                return new EOarray(newArray);
            }
            return this;
        } catch (Exception e) {
            return EOappend(obj);
        }
    }

    /**
     * Evaluates {@code evaluatorObject} against each element of this array. Results of evaluations are not considered.
     * This method always returns {@code true}. Basically, this method is useful to dataize (in other words, execute or
     * evaluate) some routine against each element of an array when results are not needed.
     *
     * @param evaluatorObject an EO object that must have an {@code each} attribute which must have a free attribute
     *                        that receives the current element being utilized by {@code evaluatorObject}.
     *                        The name of the free attribute does not matter and may be chosen freely.
     *                        The {@code each} attribute must bind an expression to be evaluated to {@code @}.
     * @return {@code true}.
     */
    public EObool EOeach(EOObject evaluatorObject) {
        for (EOObject current : _array) {
            evaluatorObject._getAttribute("EOeach", current)._getData();
        }
        return new EObool(true);
    }

    /**
     * Retrieves the element at the position {@code i} of this array.
     *
     * @param i an index of the element to be fetched.
     * @return an element at the position {@code i}.
     * @throws IndexOutOfBoundsException if {@code i} is out of bounds of this array
     *                                   (i.e., {@code array.length <= i < 0}).
     */
    public EOObject EOget(EOObject i) {
        int position = i._getData().toInt().intValue();
        if (position >= _array.size() || position < 0) {
            throw new IndexOutOfBoundsException(
                    String.format(
                            "Cannot retrieve the element at the position %d of the following array: %s. The index is out of bounds.",
                            position,
                            this
                    )
            );
        }
        return _array.get(position);
    }

    /**
     * Determines if this array is empty.
     *
     * @return {@code true} if this array is empty, otherwise {@code false}.
     */
    public EObool EOisEmpty() {
        return new EObool(_array.isEmpty());
    }

    /**
     * Retrieves the length of this array.
     *
     * @return an {@code int} representing the length of this array.
     */
    public EOint EOlength() {
        return new EOint(_array.size());
    }

    /**
     * Transforms this array in accordance with {@code mapperObject}.
     * <p>
     * This operation does not mutate the original array.
     * Instead, it produces a copy of this array where each element is transformed with {@code mapperObject}.
     *
     * @param mapperObject an EO object that must have a {@code map} attribute which must have a free attribute
     *                     that receives the current element being transformed.
     *                     The name of the free attribute does not matter and may be chosen freely.
     *                     The {@code map} attribute must bind a transformation
     *                     technique (function) to its {@code @} attribute.
     * @return an {@code array} object containing mapped elements.
     */
    public EOarray EOmap(EOObject mapperObject) {
        int length = _array.size();
        EOObject[] mappedArray = new EOObject[length];
        for (int i = 0; i < length; i++) {
            mappedArray[i] = mapperObject._getAttribute("EOmap", _array.get(i))._getDecoratedObject();
        }
        return new EOarray(mappedArray);
    }

    /**
     * Transforms this array in accordance with {@code mapperObject}.
     * This variant of mapping considers indices while transforming elements.
     * <p>
     * This operation does not mutate the original array.
     * Instead, it produces a copy of this array where each element is transformed with {@code mapperObject}.
     *
     * @param mapperObject an EO object that must have a {@code mapi} attribute which must have two free attributes:
     *                     1. The first free attribute receives the current element being transformed.
     *                     2. The second free attribute receives the index of the current element being transformed.
     *                     The order of the free attributes matters, and their names do not.
     *                     The {@code mapi} attribute must bind a transformation
     *                     technique (function) to its {@code @} attribute.
     * @return an {@code array} object containing mapped elements.
     */
    public EOarray EOmapi(EOObject mapperObject) {
        int length = _array.size();
        EOObject[] mappedArray = new EOObject[length];
        for (int i = 0; i < length; i++) {
            mappedArray[i] = mapperObject._getAttribute("EOmapi", _array.get(i), new EOint(i))._getDecoratedObject();
        }
        return new EOarray(mappedArray);
    }

    /**
     * Retrieves all pairs of the elements of this array.
     * Resulting pairs are essentially 2-combinations with no repetitions (order is not taken into account).
     * Uniqueness of elements within pairs is not guaranteed (this method, however, guarantees that resulting pairs
     * themselves are unique regarding positions of included elements), so users of this method should consider
     * eliminating duplicates before retrieving pairs if unique elements within pairs are required (see examples below).
     * <p>
     * Example #1:
     * array([1, 2, 3]).pairs -> array([tuple(1, 2), tuple(1, 3), tuple(2, 3)])
     * Example #2:
     * array([1, 2, 2]).pairs -> array([tuple(1, 2), tuple(1, 2), tuple(2, 2)])
     *
     * @return an {@code array} of {@code tuple} objects with pairs of the elements of this array.
     */
    public EOarray EOpairs() {
        return new EOarray(
                Generator.combination(this._array)
                        .simple(2)
                        .stream()
                        .map(pair -> new EOtuple(pair.get(0), pair.get(1)))
                        .toArray(size -> new EOObject[size])
        );
    }

    /**
     * Performs the operation of reduction of this array
     * (i.e., this method transforms this array into a single value in accordance with {@code reducerObject}).
     *
     * @param accumulator   an initial value of the accumulator.
     * @param reducerObject an EO object that must have a {@code reduce} attribute which must have two free attributes:
     *                      1. The first free attribute receives the current value of the accumulator.
     *                      2. The second free attribute receives the current element being operated over.
     *                      The order of the free attributes matters, and their names do not.
     *                      The {@code reduce} attribute must bind a reduction technique (function) to {@code @}.
     * @return the value of the accumulator after operating over the last element of this array (i.e., the result of reduction).
     */
    public EOObject EOreduce(EOObject accumulator, EOObject reducerObject) {
        EOObject out = accumulator;
        for (EOObject eoObject : _array) {
            out = reducerObject._getAttribute("EOreduce", out, eoObject)._getDecoratedObject();
        }
        return out;
    }

    /**
     * Performs the operation of reduction of this array
     * (i.e., this method transforms this array into a single value in accordance with {@code reducerObject}).
     * This variant of reduction considers indices while operating over elements.
     *
     * @param accumulator   an initial value of the accumulator.
     * @param reducerObject an EO object that must have a {@code reducei} attribute which must have three free attributes:
     *                      1. The first free attribute receives the current value of the accumulator.
     *                      2. The second free attribute receives the current element being operated over.
     *                      3. The third free attribute receives the index of the current element.
     *                      The order of the free attributes matters, and their names do not.
     *                      The {@code reducei} attribute must bind a reduction technique (function) to {@code @}.
     * @return the value of the accumulator after operating over the last element of this array (i.e., the result of reduction).
     */
    public EOObject EOreducei(EOObject accumulator, EOObject reducerObject) {
        EOObject out = accumulator;
        int length = _array.size();
        for (int i = 0; i < length; i++) {
            out = reducerObject._getAttribute("EOreducei", out, _array.get(i), new EOint(i))._getDecoratedObject();
        }
        return out;
    }

    /**
     * !!!For testing purposes only!!!
     * <p>
     * Determines if this array is equal to the {@code o} object.
     * To do it, this method checks that the {@code o} object is an array
     * and it contains similar elements by delegating equality checks to
     * the elements themselves.
     * <p>
     * This method can be called only in the testing environment
     * since all methods within the EO environment have the 'EO' prefix.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EOarray eOarray = (EOarray) o;
        return _array.equals(eOarray._array);
    }

    @Override
    public int hashCode() {
        return Objects.hash(_array);
    }

    /**
     * !!!For testing purposes only!!!
     * <p>
     * Produces a string that represents this array.
     * The resulting string has the following form:
     * array([elem1, elem2, elem3, elem4]),
     * where each elemN is converted to a string, too.
     * <p>
     * Example:
     * Say, an array has three int elements: 1, 2, 3.
     * Then, the string representation of the array is:
     * array([int(1), int(2), int(3)]).
     * <p>
     * This method can be called only in the testing environment
     * since all methods within the EO environment have the 'EO' prefix.
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("array([");
        for (EOObject o : _array) {
            sb.append(o.toString()).append(", ");
        }
        if (_array.size() > 0) {
            sb.delete(sb.length() - 2, sb.length());
        }
        sb.append("])");
        return sb.toString();
    }
}