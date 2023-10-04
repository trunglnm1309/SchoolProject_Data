import React from 'react'
import transferPrice from '../../helper/TransferPrice'


function OrderItem(props) {

    const { newprice, soluong, title, image } = props.data;
    return (
        <div className="order-item">
            <img src={image} alt="" className="order-item-img" />
            <p className="order-item-title">
                {title}
            </p>
            <div className="order-item-price-wrap">
                <span className="order-item-price-title">
                    Giá
                        </span>
                <span className="order-item-price">
                    {transferPrice(newprice)} đ
                        </span>
            </div>
            <div className="order-item-quantity-wrap">
                <span className="order-item-quantity-title">
                    Số lượng
                        </span>
                <span className="order-item-quantity">
                    {soluong}
                </span>
            </div>
        </div>
    )
}


export default OrderItem
