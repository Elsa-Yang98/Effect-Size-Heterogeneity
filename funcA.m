function ret = funcA(alpha, x)
    % The function calculate the value of E[eta_alpha(x+W)-x]^2
    ret = (1 + alpha^2) .* (normcdf(-alpha-x) + normcdf(x-alpha)) + x.^2 .* (normcdf(alpha-x)-normcdf(-alpha-x)) ...
         + (x-alpha) .* normpdf(alpha+x) - (x+alpha) .* normpdf(alpha-x);
end
        