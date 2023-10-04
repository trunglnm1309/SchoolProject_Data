import React from "react";
import { Link } from "react-router-dom";

const PaginationComment = (props) => {
    const { totalPage, crrpage, search, id } = props;
    function renderPagination() {
        var result = [];
        if (totalPage < 6) {
            for (let i = 1; i <= totalPage; i++) {
                if (crrpage === i)
                    result.push(
                        <li class="pagination-item pagination-item--active">
                            <Link
                                to={`${id}?page=${i}${
                                    search ? "&q=" + search : ""
                                }`}
                                class="pagination-item__link"
                            >
                                {i}
                            </Link>
                        </li>
                    );
                else
                    result.push(
                        <li class="pagination-item">
                            <Link
                                to={`${id}?page=${i}${
                                    search ? "&q=" + search : ""
                                }`}
                                class="pagination-item__link"
                            >
                                {i}
                            </Link>
                        </li>
                    );
            }
        } else {
            var start = 0;
            if (crrpage % 5 === 0) start = crrpage;
            for (let i = start; i <= start + 5; i++) {
                if (i === totalPage) break;
                if (crrpage === i)
                    result.push(
                        <li class="pagination-item pagination-item--active">
                            <Link
                                to={`${id}?page=${i}${
                                    search ? "&q=" + search : ""
                                }`}
                                class="pagination-item__link"
                            >
                                {i}
                            </Link>
                        </li>
                    );
                else
                    result.push(
                        <li class="pagination-item">
                            <Link
                                to={`${id}?page=${i}${
                                    search ? "&q=" + search : ""
                                }`}
                                class="pagination-item__link"
                            >
                                {i}
                            </Link>
                        </li>
                    );
            }
        }
        return result;
    }
    return (
        <ul class="pagination">
            <li
                to={`${id}?page=${crrpage - 1 < 1 ? 1 : crrpage - 1}${
                    search ? "&q=" + search : ""
                }`}
                class="pagination-item"
            >
                <Link
                    to={`${id}?page=${
                        crrpage + 1 > totalPage ? totalPage : crrpage + 1
                    }${search ? "&q=" + search : ""}`}
                    class="pagination-item__link"
                >
                    <i class="pagination-item__icon fas fa-chevron-left "></i>
                </Link>
            </li>
            {renderPagination()}
            <li class="pagination-item">
                <Link
                    to={`${id}?page=${totalPage}${
                        search ? "&q=" + search : ""
                    }`}
                    class="pagination-item__link"
                >
                    ...
                </Link>
            </li>
            <li class="pagination-item">
                <Link
                    to={`${id}?page=${totalPage}${
                        search ? "&q=" + search : ""
                    }`}
                    class="pagination-item__link"
                >
                    {totalPage}
                </Link>
            </li>
            <li>
                <Link
                    to={`${id}?page=${
                        crrpage + 1 > totalPage ? totalPage : crrpage + 1
                    }${search ? "&q=" + search : ""}`}
                    class="pagination-item__link"
                >
                    <i class="pagination-item__icon fas fa-chevron-right "></i>
                </Link>
            </li>
        </ul>
    );
};

export default PaginationComment;
