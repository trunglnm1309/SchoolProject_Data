import React from 'react'
import EmptyCart from '../../assets/img/Empty.png'
import { Link } from 'react-router-dom';

function CartEmpty() {
    return (
        <div class="cart-home__no">
            <img src={EmptyCart} alt="" />
            <span class="cart-home__no-notify">Giỏ hàng chưa có sản phẩm nào</span>
            <button><Link class="cart-home__no-back-home" to="/">Mua sắm ngay</Link></button>
        </div>
    )
}

export default CartEmpty
