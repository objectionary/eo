package org.eolang;

import org.eolang.core.EOObject;
import org.eolang.core.data.EODataObject;
import org.paukov.combinatorics3.Generator;

import java.util.List;
import java.util.Objects;

/***
 * Represents an array
 */
public class EOarray extends EOObject {
    private final List<EOObject> _array;

    public EOarray() {
        _array = List.of();
    }

    public EOarray(EOObject... objects) {
        _array = List.of(objects);
    }

    /***
     * Determines if the base array is empty
     * @return A boolean object, true if empty and false if not empty
     */
    public EObool EOisEmpty() {
        return new EObool(_array.isEmpty());
    }

    /***
     * Gets the length of the array
     * @return An object representing the length of the array
     */
    public EOint EOlength() {
        return new EOint(_array.size());
    }

    /***
     * Get the object at {@code position} free attribute of the array
     * @param position an index of the array to fetch from
     * @return An object representing the value at the {@code position} free attribute index of the array
     */
    public EOObject EOget(EOObject position) {
        if (_array.size() <= position._getData().toInt().intValue()) {
            throw new IllegalArgumentException(
                    String.format(
                            "Can't get() the %dth element of the array, there are just %d of them",
                            position._getData().toInt().intValue(), _array.size()
                    )
            );
        }
        return _array.get(position._getData().toInt().intValue());
    }

    /***
     * Appends the object of the free attribute {@code eoObject} to the array
     * @param eoObject An object to append to this array
     * @return An object representing the new array with the appended object of the free attribute {@code eoObject}
     */
    public EOarray EOappend(EOObject eoObject) {
        List<EOObject> newList = new java.util.ArrayList<>(_array);
        newList.add(eoObject);
        EOObject[] newArray = new EOObject[newList.size()];
        return new EOarray(newList.toArray(newArray));
    }

    /***
     * Performs the reduction operation of the base array object
     * @param accumulator a partial/subtotal result
     * @param reduceFunction represents the reduction function
     * @return An object representing the final accumulated value of the reduce operation
     */
    public EOObject EOreduce(EOObject accumulator, EOObject reduceFunction) {
        EOObject out = accumulator;
        for (EOObject eoObject : this._array) {
            out = reduceFunction._getAttribute("EOreduce", out, eoObject)._getDecoratedObject();
        }
        return out;
    }

    /***
     * Performs the reduction operation of the base array object
     * @param accumulator a partial/subtotal result
     * @param reduceiFunction represents the reduction function
     * @return An object representing the final accumulated value of the reduce operation
     */
    public EOObject EOreducei(EOObject accumulator, EOObject reduceiFunction) {
        EOObject out = accumulator;
        int length =_array.size();
        for(int i = 0; i < length;i++){
            out = reduceiFunction._getAttribute("EOreducei", out,_array.get(i),new EODataObject(i))._getDecoratedObject();
        }
        return out;
    }

    /***
     * Performs a map operation on the base array object
     * @param mapFunction represents the map function
     * @return An {@code EOarray} object containing mapped elements
     */
    public EOarray EOmap(EOObject mapFunction) {
        int length = _array.size();
        EOObject[] mappedArray = new EOObject[length];
        for (int i = 0; i < length; i++) {
            mappedArray[i] = mapFunction._getAttribute("EOmap", _array.get(i))._getDecoratedObject();
        }
        return new EOarray(mappedArray);
    }

    /***
     * Performs a map index operation on the base array object
     * @param mapiFunction represents the map function
     * @return An {@code EOarray} object containing mapped elements
     */
    public EOarray EOmapi(EOObject mapiFunction) {
        int length = _array.size();
        EOObject[] mappedArray = new EOObject[length];
        for (int i = 0; i < length; i++) {
            mappedArray[i] = mapiFunction._getAttribute("EOmapi", _array.get(i), new EOint(i))._getDecoratedObject();
        }
        return new EOarray(mappedArray);
    }

    /***
     * Applies the {@code eachFunction} to all the elements of the array
     * @param eachFunction represents the function to apply to each object of the array
     * @return EObool(True)
     */
    public EObool EOeach(EOObject eachFunction) {
        int length = _array.size();
        for (int i = 0; i < length; i++) {
            eachFunction._getAttribute("EOeach", _array.get(i))._getData();
        }
        return new EObool(true);
    }

    public EOarray EOpairs() {
        return new EOarray(Generator.combination(this._array.toArray(EOObject[]::new))
                .simple(2)
                .stream()
                .map(pair -> new EOtuple(pair.get(0), pair.get(1)))
                .toArray(EOObject[]::new));
    }

    /**
     * !!!For testing purposes only!!!
     *
     * Determines if this array is equal to the {@code o} object.
     * To do it, this method checks that the {@code o} object is an array
     * and it contains similar elements by delegating equality checks to
     * the elements themselves.
     *
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

    /**
     * !!!For testing purposes only!!!
     *
     * Produces a string that represents this array.
     * The resulting string has the following form:
     * array([elem1, elem2, elem3, elem4]),
     * where each elemN is converted to a string, too.
     *
     * Example:
     * Say, an array has three int elements: 1, 2, 3.
     * Then, the string representation of the array is:
     * array([int(1), int(2), int(3)]).
     *
     * This method can be called only in the testing environment
     * since all methods within the EO environment have the 'EO' prefix.
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("array([");
        for (EOObject o:_array) {
            sb.append(o.toString()).append(", ");
        }
        sb.delete(sb.length()-2, sb.length());
        sb.append("])");
        return sb.toString();
    }
}