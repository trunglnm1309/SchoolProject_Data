import React from 'react'
import {Link} from 'react-router-dom'
import './Breadcum.css'
function Breadcum(props) {
    const {ListBreadcum,final} = props;
    function content(){
        var result = [];
        if(ListBreadcum){

            ListBreadcum.forEach(element => {
                result.push(<span class="cart-home__filter-right">{'>'}</span>);
                result.push(<span class="cart-home__filter-current">{element}</span>);
            });
        }
        result.push(<span class="cart-home__filter-right">{'>'}</span>);
        result.push(<span class="cart-home__filter-current">{final}</span>);
        return result;
    }
    return (
        <div className="Breadcum">
        <Link to="/">
            <i class="fas fa-home cart-home__filter-back"></i>
        </Link>
        {content()}
       
    </div>
    )
}
export default Breadcum
 