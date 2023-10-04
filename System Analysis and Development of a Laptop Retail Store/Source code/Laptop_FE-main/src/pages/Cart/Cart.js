import React from 'react'
import Breadcum from '../../components/Breadcum/Breadcum'
import CartComponent from '../../components/Cart/CartComponent'
import './Cart.css'
function Cart() {
    return (
        <div className="cart-home">
            <div className="container">
                <Breadcum final={"Giỏ hàng"}/>
                <CartComponent/>
            </div>
        </div>
    )
}

export default Cart
