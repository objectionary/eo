package org.eolang;

import org.eolang.core.EOObject;
import org.eolang.core.EOObjectArray;
import org.eolang.core.data.EOData;
import org.eolang.core.data.EODataObject;

/***
 * Represents an array
 * @version %I%, %G%
 */
public class EOarray extends EOObject {
    private final EOObjectArray array;

    public EOarray(EOObjectArray array) {
        this.array = array;
    }

    @Override
    public EOData _getData() {
        return array._getData();
    }

    /***
     * Determines if the base array is empty
     * @return A boolean object, true if empty and false if not empty
     */
    public EObool EOisEmpty() {
        return new EObool(array.get_array().length == 0);
    }

    /***
     * Gets the length of the array
     * @return An object representing the length of the array
     */
    public EOint EOlength() {
        return new EOint(array.get_array().length);
    }

    /***
     * Get the object at {@code position} free attribute of the array
     * @param position an index of the array to fetch from
     * @return An object representing the value at the {@code position} free attribute index of the array
     */
    public EOObject EOget(EOObject position) {
        return array.get_array()[position._getData().toInt().intValue()];
    }

    /***
     * Appends the object of the free attribute {@code eoObject} to the array
     * @param eoObject An object to append to this array
     * @return An object representing the a new array with the appended object of the free attribute {@code eoObject}
     */
    public EOObjectArray EOappend(EOObject eoObject) {
        EOObject[] newArray = new EOObject[this.array.get_array().length + 1];
        for (int i = 0; i < this.array.get_array().length; i++) {
            newArray[i] = this.array.get_array()[i];
        }
        newArray[this.array.get_array().length] = eoObject;
        return new EOObjectArray(newArray);
    }

    /***
     * TO DO
     * Performs the reduction operation of its base array object
     * @param accumulator a partial result
     * @param reducefunction represents the reduction function
     * @return
     */
    public EOObject EOreduce(EOObject accumulator, EOObject reducefunction) {
//        TO DO
        return new EODataObject(0);
    }

    /***
     * To Do
     * @param falsy
     * @return
     */
    public EObool EOeach(EObool falsy) {
        for (EOObject eoObject : array.get_array()) {
//            TO DO
        }
        return new EObool(true);
    }

    /***
     * TO Do
     * @param falsy
     * @return
     */
    public EOObject EOmap(EObool falsy) {
//        TO DO
        return new EODataObject(-1);
    }

    /***
     * TO DO
     * @param falsy
     * @return
     */
    public EOObject EOmapi(EObool falsy) {
//        To DO
        return new EODataObject(-1);
    }


}
