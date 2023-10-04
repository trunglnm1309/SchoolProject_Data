import React from "react";
import PropTypes from "prop-types";
import { Link } from "react-router-dom";

const Pagination = (props) => {
    const { totalPage, crrpage, category, type, search } = props;
    function renderPagination() {
        var result = [];
        if (totalPage < 6) {
            for (let i = 1; i <= totalPage; i++) {
                if (crrpage === i)
                    result.push(
                        <Link
                            to={`${category !== "" ? category + "?" : ""}${
                                search ? "q=" + search + "&" : ""
                            }page=${i}${type !== "" ? "&type=" + type : ""}`}
                        >
                            <button class="home-product__page-number home-product__page-number--current ">
                                {i}
                            </button>
                        </Link>
                    );
                else
                    result.push(
                        <Link
                            to={`${category !== "" ? category + "?" : ""}${
                                search ? "q=" + search + "&" : ""
                            }page=${i}${type !== "" ? "&type=" + type : ""}`}
                        >
                            <button class="home-product__page-number">
                                {i}
                            </button>
                        </Link>
                    );
            }
        } else {
            var start = 0;
            if (crrpage % 5 === 0) start = crrpage;
            for (let i = start; i <= start + 5; i++) {
                if (i === totalPage) break;
                if (crrpage === i)
                    result.push(
                        <Link
                            to={`${category !== "" ? category + "?" : ""}${
                                search ? "q=" + search + "&" : ""
                            }page=${i}&${type !== "" ? "type=" + type : ""}`}
                        >
                            <button class="home-product__page-number home-product__page-number--current ">
                                {i}
                            </button>
                        </Link>
                    );
                else
                    result.push(
                        <Link
                            to={`${category !== "" ? category + "?" : ""}${
                                search ? "q=" + search + "&" : ""
                            }page=${i}&${type !== "" ? "type=" + type : ""}`}
                        >
                            <button class="home-product__page-number">
                                {i}
                            </button>
                            );
                        </Link>
                    );
            }
        }
        return result;
    }
    return (
        <div class="home-product__page">
            <Link
                to={`${category !== "" ? category + "?" : ""}${
                    search ? "q=" + search + "&" : ""
                }page=${crrpage - 1 < 1 ? 1 : crrpage - 1}&${
                    type !== "" ? "type=" + type : ""
                }`}
            >
                <button
                    href=""
                    class="home-product__page-control home-product__page-control-left"
                >
                    <i class="fas fa-chevron-left"></i>
                </button>
            </Link>
            {renderPagination()}
            <Link
                to={`${category !== "" ? category + "?" : ""}${
                    search ? "q=" + search + "&" : ""
                }page=${totalPage}&${type !== "" ? "type=" + type : ""}`}
            >
                <button class="home-product__page-number">...</button>
            </Link>
            <Link
                to={`${category !== "" ? category + "?" : ""}${
                    search ? "q=" + search + "&" : ""
                }page=${totalPage}&${type !== "" ? "type=" + type : ""}`}
            >
                <button class="home-product__page-number">{totalPage}</button>
            </Link>
            <Link
                to={`${category !== "" ? category + "?" : ""}${
                    search ? "q=" + search + "&" : ""
                }page=${crrpage + 1 > totalPage ? totalPage : crrpage + 1}&${
                    type !== "" ? "type=" + type : ""
                }`}
            >
                <button
                    href=""
                    class="home-product__page-control home-product__page-control-right"
                >
                    <i class="fas fa-chevron-right"></i>
                </button>
            </Link>
        </div>
    );
};

Pagination.propTypes = {};

export default Pagination;
