
function percentage(a, x, y, minA, maxA) {

    // console.log( a.perimeter, x, y, minA, maxA );

    // Ensure 'a' is positive to avoid NaN results
    
    var p = minA;
    
    if ( a.perimeter <= minA ) { p = minA; } else { p = a.perimeter; }

    var pct = 1 - ((Math.log10(p - minA + 1) / Math.log10(maxA - minA + 1)) * (y - x) + x);
    
    return pct;
    
    // console.log( pct );
      
}

exports.percentage = percentage