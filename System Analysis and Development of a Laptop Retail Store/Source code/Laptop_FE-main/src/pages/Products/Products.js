import React, { useEffect, useState } from "react";
import Breadcum from "../../components/Breadcum/Breadcum";
import "./Products.css";
import ProductSort from "../../components/Products/ProductSort";
import ProductItem from "./../../components/Products/ProductItem";
import Pagination from "../../components/Pagination/Pagination";
import { useLocation } from "react-router-dom";
import Loader from "react-loader-spinner";
import CallApi from "../../helper/axiosClient";
function useQuery() {
    return new URLSearchParams(useLocation().search);
}
const Products = (props) => {
    const [option, setoption] = useState({
        sort: null,
        max: null,
        min: null,
    });
    const [ListProduct, setListProduct] = useState({
        isLoading: false,
        data: null,
    });
    const [totalPage, settotalPage] = useState(null);
    const query = useQuery();
    const page = query.get("page") || 1;
    const { category } = props.match.params;
    const type = query.get("type") || 1;
    const value = query.get("q") || null;
    useEffect(() => {
        const getProduct = async () => {
            setListProduct({ ...ListProduct, isLoading: true });
            var Data;
            if (category === "search") {
                Data = await CallApi({
                    url: `/api/products?search=${value}${
                        option.sort ? "&sort=" + option.sort : ""
                    }${option.max ? "&max=" + option.max : ""}${
                        option.min ? "&min=" + option.min : ""
                    }`,
                    method: "get",
                });
            } else {
                Data = await CallApi({
                    url: `/api/products/${category}?${
                        page ? "page=" + page : ""
                    }&${type ? "type=" + type : ""}${
                        option.sort ? "&sort=" + option.sort : ""
                    }${option.max ? "&max=" + option.max : ""}${
                        option.min ? "&min=" + option.min : ""
                    }`,
                    method: "get",
                });
            }
            setListProduct({ data: Data.listProducts, isLoading: false });
            settotalPage(Data.totalPage);
        };
        getProduct();
    }, [props.location, option]);
    useEffect(() => {
        window.scrollTo({
            top: 0,
            behavior: "smooth",
        });
    });
    function changeOption({ max, min, sort }) {
        setoption({
            ...option,
            max,
            min,
            sort,
        });
    }
    function handleRenderItem(ListProduct) {
        var result = [];
        if (ListProduct) {
            result = ListProduct.map((item, stt) => (
                <ProductItem item={item} key={stt}></ProductItem>
            ));
        }
        return result;
    }
    return (
        <div class="body">
            <div class="grid">
                <Breadcum final={category}></Breadcum>
                <ProductSort
                    category={category}
                    type={type}
                    search={value}
                    changeOption={changeOption}
                ></ProductSort>
            </div>
            <div class="home-product">
                <div class="grid">
                    <div class="grid__row">
                        {ListProduct.isLoading ? (
                            <Loader
                                type="Circles"
                                color="#f50057"
                                height={100}
                                width={100}
                                style={{ textAlign: "center", width: "100%" }}
                            />
                        ) : (
                            handleRenderItem(ListProduct.data)
                        )}
                    </div>
                </div>
            </div>
            <Pagination
                totalPage={totalPage}
                crrpage={parseInt(page)}
                category={category}
                type={type}
                search={value}
            ></Pagination>
        </div>
    );
};

Products.propTypes = {};

export default Products;
