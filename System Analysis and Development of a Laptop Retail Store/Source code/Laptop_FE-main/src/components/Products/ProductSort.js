import React, { useState } from "react";
import PropTypes from "prop-types";
import { Link, useRouteMatch } from "react-router-dom";
const ProductSort = (props) => {
    const { category, search, changeOption } = props;
    const { type } = props;
    const [value, setvalue] = useState({
        min: null,
        max: null,
        sort: null,
    });
    function onClickSort(e) {
        changeOption({
            ...value,
            [e.target.name]: e.target.value,
        });
        setvalue({
            ...value,
            [e.target.name]: e.target.value,
        });
    }

    function onChangeValue(e) {
        setvalue({
            ...value,
            [e.target.name]: e.target.value,
        });
    }
    function onClickFilter() {
        changeOption({
            ...value,
        });
    }
    return (
        <div class="home__filter">
            <span class="home__filter-lable">Sắp xếp theo</span>
            <div class="home__filter-sort">
                {!search && (
                    <div>
                        <Link
                            to={`${category}?${
                                search ? "q=" + search + "&" : ""
                            }page=1&type=1`}
                        >
                            <button
                                className={`home__filter-sort-btn ${
                                    type === 1
                                        ? "home__filter-sort-btn-select"
                                        : ""
                                }`}
                            >
                                Mới về
                                {type === 1 ? (
                                    <span class="home__filter-sort-btn-check">
                                        ✓
                                    </span>
                                ) : (
                                    ""
                                )}
                            </button>
                        </Link>
                        <Link
                            to={`${category}?${
                                search ? "q=" + search + "&" : ""
                            }page=1&type=2`}
                        >
                            <button
                                className={`home__filter-sort-btn ${
                                    type === 2
                                        ? "home__filter-sort-btn-select"
                                        : ""
                                }`}
                            >
                                Khuyến mãi tốt nhất
                                {type === 2 ? (
                                    <span class="home__filter-sort-btn-check">
                                        ✓
                                    </span>
                                ) : (
                                    ""
                                )}
                            </button>
                        </Link>
                        <Link
                            to={`${category}?${
                                search ? "q=" + search + "&" : ""
                            }page=1&type=3`}
                        >
                            <button
                                className={`home__filter-sort-btn ${
                                    type === 3
                                        ? "home__filter-sort-btn-select"
                                        : ""
                                }`}
                            >
                                Bán chạy
                                {type === 3 ? (
                                    <span class="home__filter-sort-btn-check">
                                        ✓
                                    </span>
                                ) : (
                                    ""
                                )}
                            </button>
                        </Link>
                    </div>
                )}
                <button
                    className={`home__filter-sort-btn ${
                        value.sort === -1 ? "home__filter-sort-btn-select" : ""
                    }`}
                    name="sort"
                    value={-1}
                    onClick={onClickSort}
                >
                    Giá giảm dần
                    {value.sort === -1 ? (
                        <span class="home__filter-sort-btn-check">✓</span>
                    ) : (
                        ""
                    )}
                </button>
                <button
                    className={`home__filter-sort-btn ${
                        value.sort === 1 ? "home__filter-sort-btn-select" : ""
                    }`}
                    name="sort"
                    value={1}
                    onClick={onClickSort}
                >
                    Giá tăng dần
                    {value.sort === 1 ? (
                        <span class="home__filter-sort-btn-check">✓</span>
                    ) : (
                        ""
                    )}
                </button>
            </div>
            <div class="home__filter-price">
                <input
                    type="text"
                    id=""
                    class="typing-1"
                    value={value.min}
                    placeholder="Giá thấp nhất"
                    name="min"
                    onChange={onChangeValue}
                />
                <span class="space">-</span>
                <input
                    type="text"
                    id=""
                    class="typing-2"
                    value={value.max}
                    placeholder="Giá cao nhất"
                    name="max"
                    onChange={onChangeValue}
                />
                <button
                    type="submit"
                    class="home__filter-sort-btn search-item"
                    onClick={onClickFilter}
                >
                    Tìm
                </button>
            </div>
        </div>
    );
};

ProductSort.propTypes = {};

export default ProductSort;
