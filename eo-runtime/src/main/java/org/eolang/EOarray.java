package org.eolang;

import org.eolang.core.EOObject;
import org.eolang.core.data.EODataObject;

import java.util.List;

/***
 * Represents an array
 * @version %I%, %G%
 */
public class EOarray extends EOObject {
    private final List<EOObject> _array;

    public EOarray(){
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
        for (EOObject eoObject: this._array) {
            out = reduceFunction._getAttribute("EOreduce", out, eoObject);
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
        for (int i=0;i<length;i++) {
            mappedArray[i] = mapFunction._getAttribute("EOmap", _array.get(i));
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
        for (int i=0;i<length;i++) {
            mappedArray[i] = mapiFunction._getAttribute("EOmapi", _array.get(i), new EODataObject(i));
        }
        return new EOarray(mappedArray);
    }

}
