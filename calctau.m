function tau = calctau(alpha, delta, eps, gamma, M)
    %The function calculate the value of tau via bi-search.
    %
    %The argument given are the values of alpha, delta = n/p, eps = k/p, 
    %gamma, M, and the interval of search: a, b.
    %
    %Ideally, gamma should take a sequence of gammas add to 1, are the prob
    %of each magnitude in sequence M. M is the sequence of magnitudes of
    %non-zero signals. If they are only one number, take gamma = 1, M = 50
    %as default. The upper curve only need this.
    
    
    %a0 = max(calcalpha0(delta), 0);
    %a = a0 + 1e-2;
    a = 1
    b = 1000;
        
   while res(alpha, a, eps, delta, gamma, M) * res(alpha, b, eps, delta, gamma, M) > 0
        %a = a0 + (a - a0) * 0.1;
        a = a *0.1;
        b = b * 10;
    end
    e = 1.0e-6;
    tau = (a + b)/2;
    n = 0; %the time of search.
    
    while (b - a) >= e
        if n > 100 % take too long!!
            tau;
            return
        end
        % If the res is already small
        if abs(res(alpha, tau, eps, delta, gamma, M)) < e
            tau;
            return
        % If the zero is on the right
        elseif res(alpha, a, eps, delta, gamma, M) * res(alpha, tau, eps, delta, gamma, M) > 0 
            a = tau;
            tau = (a + b) / 2;
            n = n + 1;
        % If the zero is on the left
        elseif res(alpha, b, eps, delta, gamma, M) * res(alpha, tau, eps, delta, gamma, M) > 0 
            b = tau;
            tau =(a + b) / 2;
            n = n + 1;
        end
    end
end