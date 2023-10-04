const transferPrice = (price) => {
    var StringPrice = "";
    var temp = 0;
    var clonePrice = price;
    while (clonePrice != 0) {
        var LastNumber = clonePrice % 10;
        clonePrice = parseInt(clonePrice / 10);
        temp++;
        StringPrice = LastNumber + StringPrice;
        if (temp === 3 && clonePrice != 0) {
            StringPrice = "." + StringPrice;
            temp = 0;
        }
    }
    return StringPrice;
};
export default transferPrice;
