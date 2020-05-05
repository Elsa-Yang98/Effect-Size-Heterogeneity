function alpha0 = calcalpha0(delta)
    %The function calculate the value of alpha0 via bi-search.
    %alpha0 is the solution of (1 + t^2) Phi(-t) - t phi(t) = delta/2

    a = -1;
    b = 1;
        
    if evaluate(delta, a) * evaluate(delta, b) > 0
        a = a * 0.1;
        b = b + 10;
    end
    e = 1.0e-6;
    alpha0 = (a + b)/2;
    n = 0; %the time of search.
    
    while true 
        if n > 100 % take too long!!
            warning('Searching time over 100, zero may be incorrect.')
            return
        end
        % If the res is already small
        if abs(evaluate(delta, alpha0)) < e
            return
        % If the zero is on the right
        elseif evaluate(delta, a) * evaluate(delta, alpha0) > 0 
            a = alpha0;
            alpha0 = (a + b) / 2;
            n = n + 1;
        % If the zero is on the left
        elseif evaluate(delta , b) * evaluate(delta, alpha0) > 0 
            b = alpha0;
            alpha0 =(a + b) / 2;
            n = n + 1;
        end
    end
end

function result = evaluate(delta, t)
    result = (1 + t^2) * normcdf(-t) - t * normpdf(t) - delta / 2;
end



