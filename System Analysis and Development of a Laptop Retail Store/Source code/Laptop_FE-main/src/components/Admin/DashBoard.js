import React, { useEffect, useLayoutEffect, useMemo, useState } from "react";
import { Chart as ChartJS, registerables } from "chart.js";
import { Bar } from "react-chartjs-2";
import AxiosInstance from "../../helper/axiosClient";
import Loader from "react-loader-spinner";

const Charjs = (props) => {
    useLayoutEffect(() => {
        ChartJS.register(...registerables);
    }, []);

    const { data, options, title } = props;
    return (
        <>
            <div className="header">
                <h1 className="title text-center" style={{ color: "#000" }}>
                    {title}
                </h1>
            </div>
            <Bar data={data} options={options} />
        </>
    );
};

const options = {
    responsive: true,
    maintainAspectRatio: false,
    pan: {
        enabled: true,
        mode: "xy",
    },
    zoom: {
        enabled: true,
        mode: "xy",
    },
    skipNull: true,
};

function DashBoard() {
    const [Products, setProducts] = useState(null);
    const [Order, setOrder] = useState(null);
    useEffect(() => {
        async function fetchData() {
            const ResponseProduct = await AxiosInstance({
                url: "/api/admin/products",
                method: "get",
            });
            const ResponseOrder = await AxiosInstance({
                url: "/api/auth/orders",
                method: "get",
            });
            setProducts(ResponseProduct);
            setOrder(ResponseOrder);
        }
        fetchData();
    }, []);
    const RenderContent = () => {
        let temp = [];
        const dataProduct = {
            labels: ["Tất cả", "laptop", "Màn Hình", "Chuột", "PC"],
            datasets: [
                {
                    label: "Sản phẩm",
                    data: (() => {
                        var arrayTemp = [Products.length, 0, 0, 0, 0];
                        Products.forEach((element) => {
                            if (element.category === "Laptop") {
                                arrayTemp[1]++;
                            } else if (element.category === "Chuột") {
                                arrayTemp[3]++;
                            } else if (element.category === "Màn Hình") {
                                arrayTemp[2]++;
                            } else if (element.category === "PC") {
                                arrayTemp[4]++;
                            }
                        });
                        return arrayTemp;
                    })(),
                    backgroundColor: [
                        "#6cd482",
                        "#e3fe04",
                        "#198754",
                        "#dc3545",
                    ],
                    borderColor: ["#6cd482", "#e3fe04", "#46932c", "#dc3545"],
                    border: 1,
                },
            ],
        };
        const dataOrder = {
            labels: ["Tất cả", "Chờ Duyệt", "Đã duyệt", "Đã hủy"],
            datasets: [
                {
                    label: "Đơn Hàng",
                    data: (() => {
                        var arrayTemp = [Order.length, 0, 0, 0];
                        Order.forEach((element) => {
                            if (element.cancelreason) {
                                arrayTemp[3]++;
                            } else if (element.status_order) {
                                arrayTemp[2]++;
                            } else {
                                arrayTemp[1]++;
                            }
                        });
                        return arrayTemp;
                    })(),
                    backgroundColor: [
                        "#0d6efd",
                        "#6c757d",
                        "#46932c",
                        "#bdb485",
                        "#add1a2",
                    ],
                    borderColor: [
                        "#0d6efd",
                        "#6c757d",
                        "46932c",
                        "#bdb485",
                        "#add1a2",
                    ],
                    border: 1,
                },
            ],
        };
        temp.push(
            <Charjs
                data={dataOrder}
                options={options}
                title="Thống kê đơn hàng"
            />
        );
        temp.push(
            <Charjs
                data={dataProduct}
                options={options}
                title="Thống kê sản phẩm"
            />
        );
        return temp;
    };
    return (
        <div style={{ height: "100vh", overflow: "scroll" }}>
            {Order && Products ? (
                RenderContent()
            ) : (
                <Loader
                    type="Circles"
                    color="#f50057"
                    height={100}
                    width={100}
                    style={{
                        position: "relative",
                        top: "40%",
                        textAlign: "center",
                        width: "100%",
                    }}
                />
            )}
        </div>
    );
}

export default DashBoard;
