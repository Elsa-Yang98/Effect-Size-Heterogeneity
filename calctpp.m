function tpp = calctpp(alpha, delta, eps, gamma, M)
    %{
    function epsi = epsilonDT(delta)
        minus_f = @(x)-(1+2/delta*x*normpdf(x) - 2/delta*(1+x^2)*normcdf(-x))/(1+x^2-2*(1+x^2)*normcdf(-x)+2*x*normpdf(x))*delta;
        alpha_phase = fminbnd(minus_f, 0, 8);
        epsi = -feval(minus_f, alpha_phase);
        return
    end

    epsilon_star = epsilonDT(delta);
    if eps <= epsilon_star
        tpp = 1;
    return;
    end
    %}
    tau = calctau(alpha, delta, eps, gamma, M);
    tpp = gamma * (1 - normcdf(alpha - M/tau) + normcdf(- alpha - M/tau))';
end
